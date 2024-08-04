(defpackage :lem-server
  (:use :cl
        :alexandria)
  (:local-nicknames (:replica :lem-server/replica))
  (:export :main))
(in-package :lem-server)

(defvar *port-counter* 10000)

;; connection with browser
(defvar *external-server* (jsonrpc:make-server))

(defun run-external-jsornpc-server (port)
  (jsonrpc:expose *external-server* "login" 'rpc/login)
  (jsonrpc:expose *external-server* "input" 'rpc/input)
  (jsonrpc:expose *external-server* "redraw" 'rpc/redraw)
  (jsonrpc:expose *external-server* "git-clone" 'rpc/git-clone)
  (jsonrpc:expose *external-server* "get-current-directory-info" 'rpc/get-current-directory-info)
  (jsonrpc:server-listen *external-server* :mode :websocket :port port :host "0.0.0.0"))

;;;
(defun rpc/login (params)
  (vom:info "login: ~A" (com.inuoe.jzon:stringify params))
  (let ((http-port (incf *port-counter*))
        (user-id (gethash "userId" params)))
    (vom:info "http-port: ~A" http-port)

    (when-let (replica (replica:find-replica-by-user-id user-id))
      (vom:info "found replica: user-id = ~A" user-id)
      (replica:add-replica-connection replica jsonrpc/connection:*connection*)
      (return-from rpc/login
        (jsonrpc:call (replica:replica-client replica)
                      "login"
                      params)))

    (let ((replica (replica:run-replica :http-port http-port
                               :user-id user-id)))
      (vom:info "created replica")

      (loop :repeat 5
            :do (sleep 1)
                (handler-case
                    (jsonrpc:client-connect (replica:replica-client replica)
                                            :mode :local-domain-socket
                                            :address (replica:replica-address replica))
                  (error (e)
                    (vom:info "try connection error: ~A" e))
                  (:no-error (&rest values)
                    (declare (ignore values))
                    (return)))
            :finally (error "Could not connect to replica"))
      (vom:info "connected")

      (replica:add-replica-connection replica jsonrpc/connection:*connection*)
      (replica:expose-replica-methods replica)

      (prog1 (jsonrpc:call (replica:replica-client replica)
                           "login"
                           params)
        (vom:info "connect succeeded")))))

(defun notify-proxy (method params)
  (let ((replica (replica:find-replica-by-connection jsonrpc/connection:*connection*)))
    (assert replica)
    (jsonrpc:notify (replica:replica-client replica) method params)))

(defun rpc/input (params)
  (notify-proxy "input" params))

(defun rpc/redraw (params)
  (notify-proxy "redraw" params))

(defun rpc/git-clone (params)
  (notify-proxy "git-clone" params))

(defun rpc/get-current-directory-info (params)
  (notify-proxy "get-current-directory-info" params))

;;;
(defun main (&optional (args (uiop:command-line-arguments)))
  (declare (ignore args))

  (vom:config t :info)

  (micros:create-server :dont-close t :port 4005 :interface "0.0.0.0")

  (bt2:make-thread (lambda () (lem-co-editing/server:start "/tmp/lem-co-editing-jsonrpc")))
  (lem-server/http-server:run 50001)
  (run-external-jsornpc-server 50000))
