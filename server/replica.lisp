(defpackage :lem-server/peer
  (:use :cl
        :alexandria)
  (:export :peer-http-port
           :peer-client
           :peer-address
           :find-peer-by-connection
           :find-peer-by-user-id
           :add-peer-connection
           :expose-peer-methods
           :run-peer
           :all-peers))
(in-package :lem-server/peer)

(defvar *peers-lock* (bt2:make-lock :name "*peers* lock"))
(defvar *peers* '())

(defun all-peers ()
  (bt2:with-lock-held (*peers-lock*)
    *peers*))

(defun add-peer (peer)
  (bt2:with-lock-held (*peers-lock*)
    (push peer *peers*)))

(defun remove-peer (peer)
  (bt2:with-lock-held (*peers-lock*)
    (deletef *peers* peer)))

(defclass peer ()
  ((process :initarg :process
            :reader peer-process)
   (output-thread :initform nil
                  :accessor peer-output-thread)
   (observe-thread :initform nil
                   :accessor peer-observe-thread)
   (http-port :initarg :http-port
              :reader peer-http-port)
   (user-id :initarg :user-id
            :reader peer-user-id)
   (address :initarg :address
            :reader peer-address)
   (client :initform (jsonrpc:make-client)
           :reader peer-client)
   (connections :initform '()
                :writer set-peer-connections
                :reader peer-connections)
   (connections-lock :initform (bt2:make-lock :name "peer-connections-lock")
                     :reader peer-connections-lock)))

(defmethod com.inuoe.jzon:coerced-fields ((peer peer))
  `(("id" ,(peer-user-id peer))))

(defmethod peer-connections-with-mutex ((peer peer))
  (bt2:with-lock-held ((peer-connections-lock peer))
    (peer-connections peer)))

(defun find-peer-by-connection (connection)
  (dolist (peer (all-peers))
    (when (member connection (peer-connections-with-mutex peer))
      (return peer))))

(defun remove-peer-by-connection (connection)
  (dolist (peer (all-peers))
    (bt2:with-lock-held ((peer-connections-lock peer))
      (when (member connection (peer-connections peer))
        (set-peer-connections (delete connection (peer-connections peer))
                                 peer)))))

(defun find-peer-by-user-id (user-id)
  (find user-id (all-peers) :key #'peer-user-id :test #'equal))

(defun add-peer-connection (peer connection)
  (bt2:with-lock-held ((peer-connections-lock peer))
    (set-peer-connections (cons connection (peer-connections peer))
                             peer)))

(defun call-peer-method (peer method params)
  (dolist (connection (peer-connections-with-mutex peer))
    (let ((jsonrpc/connection:*connection* connection))
      (jsonrpc:notify-to lem-co-editing/server::*server*
                         connection
                         method
                         params))))

(defun expose-peer-method (peer method kind)
  (jsonrpc:expose (peer-client peer)
                  method
                  (ecase kind
                    (:narrowcast
                     (lambda (params)
                       (call-peer-method peer method params)))
                    (:broadcast
                     (lambda (params)
                       (dolist (peer (all-peers))
                         (call-peer-method peer method params)))))))

(defun expose-peer-methods (peer)
  (loop :for (method kind) :in
           '(("startup" :narrowcast)
             ("update-foreground" :narrowcast)
             ("update-background" :narrowcast)
             ("make-view" :narrowcast)
             ("delete-view" :narrowcast)
             ("resize-view" :narrowcast)
             ("move-view" :narrowcast)
             ("clear" :narrowcast)
             ("clear-eol" :narrowcast)
             ("clear-eob" :narrowcast)
             ("put" :narrowcast)
             ("modeline-put" :narrowcast)
             ("update-display" :narrowcast)
             ("move-cursor" :narrowcast)
             ("resize-display" :narrowcast)
             ("bulk" :narrowcast)
             ("user-enter" :broadcast)
             ("user-exit" :broadcast)
             ("exit" :narrowcast)
             ("finish-git-clone" :broadcast)
             ("user-input" :broadcast))
        :do (expose-peer-method peer method kind)))

(defun find-lem-program ()
  (if (uiop:file-exists-p (merge-pathnames "lem-replica" (probe-file ".")))
      "./lem-replica"
      "/work/lem-server/lem-replica"))

(defun run-peer (&key http-port user-id)
  (let ((address (format nil "/tmp/jsonrpc-~A" user-id)))
    (uiop:delete-file-if-exists address)
    (let* ((command (list (find-lem-program)
                          "--mode" "local-domain-socket"
                          "--address" address
                          "--http-port" (princ-to-string http-port)
                          "--co-editing-address" "/tmp/lem-co-editing-jsonrpc"))
           (process (async-process:create-process command))
           (peer (make-instance 'peer
                                :process process
                                :http-port http-port
                                :user-id user-id
                                :address address)))
      (add-peer peer)

      (vom:info "run-peer command: ~S" command)

      (setf (peer-output-thread peer)
            (bt2:make-thread
             (lambda ()
               (loop
                 (unless (async-process:process-alive-p process)
                   (vom:info "RIP: ~A" user-id)
                   (remove-peer peer)
                   (return))
                 (let ((string (async-process:process-receive-output process)))
                   (when string
                     (dolist (string (uiop:split-string string :separator '(#\newline)))
                       (vom:info "~A: ~A" user-id string))))))
             :name (format nil "peer output thread (user-id=~S)" user-id)))
      (setf (peer-observe-thread peer)
            (bt2:make-thread
             (lambda ()
               (loop
                 (sleep 10)
                 (unless (peer-connections-with-mutex peer)
                   (vom:info "observer (user-id=~S): No connection" user-id)
                   ;; 3秒以上connectionが接続されないならば
                   (when (<= 3 (let ((n (loop :repeat 3
                                              :while (null (peer-connections-with-mutex peer))
                                              :count 1
                                              :do (sleep 1))))
                                 (vom:info "observer (user-id=~S): No connection count ~D" user-id n)
                                 n))
                     ;; プロセスを殺す
                     (async-process:delete-process process)
                     (return)))))
             :name (format nil "peer observe thread (user-id=~S)" user-id)))

      peer)))

;; TODO: server classを継承して、上書きしないようにする
(defmethod jsonrpc/base:on-close-connection :after ((jsonrpc:server jsonrpc:server) connection)
  (remove-peer-by-connection connection))
