(defpackage :lem-server/replica
  (:use :cl
        :alexandria)
  (:export :replica-http-port
           :replica-client
           :replica-address
           :find-replica-by-connection
           :find-replica-by-user-id
           :add-replica-connection
           :expose-replica-methods
           :run-replica
           :all-replicas))
(in-package :lem-server/replica)

(defvar *replicas-lock* (bt2:make-lock :name "*replicas* lock"))
(defvar *replicas* '())

(defun all-replicas ()
  (bt2:with-lock-held (*replicas-lock*)
    *replicas*))

(defun add-replica (replica)
  (bt2:with-lock-held (*replicas-lock*)
    (push replica *replicas*)))

(defun remove-replica (replica)
  (bt2:with-lock-held (*replicas-lock*)
    (deletef *replicas* replica)))

(defclass replica ()
  ((process :initarg :process
            :reader replica-process)
   (output-thread :initform nil
                  :accessor replica-output-thread)
   (observe-thread :initform nil
                   :accessor replica-observe-thread)
   (http-port :initarg :http-port
              :reader replica-http-port)
   (user-id :initarg :user-id
            :reader replica-user-id)
   (address :initarg :address
            :reader replica-address)
   (client :initform (jsonrpc:make-client)
           :reader replica-client)
   (connections :initform '()
                :writer set-replica-connections
                :reader replica-connections)
   (connections-lock :initform (bt2:make-lock :name "replica-connections-lock")
                     :reader replica-connections-lock)))

(defmethod com.inuoe.jzon:coerced-fields ((replica replica))
  `(("id" ,(replica-user-id replica))))

(defmethod replica-connections-with-mutex ((replica replica))
  (bt2:with-lock-held ((replica-connections-lock replica))
    (replica-connections replica)))

(defun find-replica-by-connection (connection)
  (dolist (replica (all-replicas))
    (when (member connection (replica-connections-with-mutex replica))
      (return replica))))

(defun remove-replica-by-connection (connection)
  (dolist (replica (all-replicas))
    (bt2:with-lock-held ((replica-connections-lock replica))
      (when (member connection (replica-connections replica))
        (set-replica-connections (delete connection (replica-connections replica))
                                 replica)))))

(defun find-replica-by-user-id (user-id)
  (find user-id (all-replicas) :key #'replica-user-id :test #'equal))

(defun add-replica-connection (replica connection)
  (bt2:with-lock-held ((replica-connections-lock replica))
    (set-replica-connections (cons connection (replica-connections replica))
                             replica)))

(defun call-replica-method (replica method params)
  (dolist (connection (replica-connections-with-mutex replica))
    (let ((jsonrpc/connection:*connection* connection))
      (jsonrpc:notify-to lem-co-editing/server::*server*
                         connection
                         method
                         params))))

(defun expose-replica-method (replica method kind)
  (jsonrpc:expose (replica-client replica)
                  method
                  (ecase kind
                    (:narrowcast
                     (lambda (params)
                       (call-replica-method replica method params)))
                    (:broadcast
                     (lambda (params)
                       (dolist (replica (all-replicas))
                         (call-replica-method replica method params)))))))

(defun expose-replica-methods (replica)
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
        :do (expose-replica-method replica method kind)))

(defun find-lem-program ()
  (if (uiop:file-exists-p (merge-pathnames "lem-replica" (probe-file ".")))
      "./lem-replica"
      "/work/lem-server/lem-replica"))

(defun run-replica (&key http-port user-id)
  (let ((address (format nil "/tmp/jsonrpc-~A" user-id)))
    (uiop:delete-file-if-exists address)
    (let* ((command (list (find-lem-program)
                          "--mode" "local-domain-socket"
                          "--address" address
                          "--http-port" (princ-to-string http-port)
                          "--co-editing-address" "/tmp/lem-co-editing-jsonrpc"))
           (process (async-process:create-process command))
           (replica (make-instance 'replica
                                :process process
                                :http-port http-port
                                :user-id user-id
                                :address address)))
      (add-replica replica)

      (vom:info "run-replica command: ~S" command)

      (setf (replica-output-thread replica)
            (bt2:make-thread
             (lambda ()
               (loop
                 (unless (async-process:process-alive-p process)
                   (vom:info "RIP: ~A" user-id)
                   (remove-replica replica)
                   (return))
                 (let ((string (async-process:process-receive-output process)))
                   (when string
                     (dolist (string (uiop:split-string string :separator '(#\newline)))
                       (vom:info "~A: ~A" user-id string))))))
             :name (format nil "replica output thread (user-id=~S)" user-id)))
      (setf (replica-observe-thread replica)
            (bt2:make-thread
             (lambda ()
               (loop
                 (sleep 10)
                 (unless (replica-connections-with-mutex replica)
                   (vom:info "observer (user-id=~S): No connection" user-id)
                   ;; 3秒以上connectionが接続されないならば
                   (when (<= 3 (let ((n (loop :repeat 3
                                              :while (null (replica-connections-with-mutex replica))
                                              :count 1
                                              :do (sleep 1))))
                                 (vom:info "observer (user-id=~S): No connection count ~D" user-id n)
                                 n))
                     ;; プロセスを殺す
                     (async-process:delete-process process)
                     (return)))))
             :name (format nil "replica observe thread (user-id=~S)" user-id)))

      replica)))

;; TODO: server classを継承して、上書きしないようにする
(defmethod jsonrpc/base:on-close-connection :after ((jsonrpc:server jsonrpc:server) connection)
  (remove-replica-by-connection connection))
