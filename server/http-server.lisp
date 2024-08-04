(defpackage :lem-server/http-server
  (:use :cl
        :alexandria)
  (:local-nicknames (:replica :lem-server/replica))
  (:export :run))
(in-package :lem-server/http-server)

(defvar *app* (make-instance 'ningle:app))

(setf (ningle:route *app* "/ping" :method :GET) 'ping
      (ningle:route *app* "/users" :method :GET) 'get-users
      (ningle:route *app* "/idle-time" :method :GET) 'get-idle-time
      ;; (ningle:route *app* "/login" :method :GET) 'login
      (ningle:route *app* "*" :method :GET) 'index)

(defmethod lack.component:call :around (component env)
  (setf (getf (lack.response:response-headers ningle:*response*) :Access-Control-Allow-Origin)
        "*"
        (getf (lack.response:response-headers ningle:*response*) :Access-Control-Allow-Headers)
        "GET")
  (call-next-method))

(defun ping (params)
  (declare (ignore params))
  "pong")

(defun get-users (params)
  (declare (ignore params))
  `(200 (:content-type "application/json")
        ,(babel:string-to-octets
          (com.inuoe.jzon:stringify
           (coerce (replica:all-replicas) 'vector)))))

(defun get-idle-time (params)
  (declare (ignore params))
  ;; TODO
  "0")

(defun index (params)
  (declare (ignore params))
  (let ((path (lack.request:request-path-info ningle:*request*)))
    (cond ((equal path "/")
           `(200 (:content-type "text/html")
                 ,(asdf:system-relative-pathname :lem-server #p"../editor/dist/index.html")))
          ((alexandria:starts-with-subseq "/assets/" path)
           (let ((path (asdf:system-relative-pathname :lem-server
                                                      (format nil
                                                              "../editor/dist/~A"
                                                              (string-left-trim "/" path)))))
             `(200 (:content-type "application/javascript") ,path))))))

(defvar *port-counter* 50002)

(defun next-port ()
  (prog1 *port-counter*
    (incf *port-counter*)))

(defun run (port)
  (clack:clackup (lack:builder :accesslog *app*)
                 :port port
                 :address "0.0.0.0"))

;; TODO: replicaのconnectionはいらなくなりそう
