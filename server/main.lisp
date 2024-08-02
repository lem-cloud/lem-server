(defpackage :lem-server
  (:use :cl
        :alexandria)
  (:local-nicknames (:peer :lem-server/peer))
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

    (when-let (peer (peer:find-peer-by-user-id user-id))
      (vom:info "found peer: user-id = ~A" user-id)
      (peer:add-peer-connection peer jsonrpc/connection:*connection*)
      (return-from rpc/login
        (jsonrpc:call (peer:peer-client peer)
                      "login"
                      params)))

    (let ((peer (peer:run-peer :http-port http-port
                               :user-id user-id)))
      (vom:info "created peer")

      (sleep 1)
      ;; TODO: ここで前のsleepが足らずに接続に失敗するケースがある
      (jsonrpc:client-connect (peer:peer-client peer)
                              :mode :local-domain-socket
                              :address (peer:peer-address peer))
      (vom:info "connected")
      (sleep 1)

      (peer:add-peer-connection peer jsonrpc/connection:*connection*)
      (peer:expose-peer-methods peer)

      (prog1 (jsonrpc:call (peer:peer-client peer)
                           "login"
                           params)
        (vom:info "connect succeeded")))))

(defun notify-proxy (method params)
  (let ((peer (peer:find-peer-by-connection jsonrpc/connection:*connection*)))
    (assert peer)
    (jsonrpc:notify (peer:peer-client peer) method params)))

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
