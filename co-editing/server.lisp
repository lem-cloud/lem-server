(defpackage :lem-co-editing/server
  (:use :cl :lem-co-editing/utils)
  (:export :start
           :expose))
(in-package :lem-co-editing/server)

;; TODO:
;; - clientの数が色の数を越えたとき
;; - clientが切断したとき

(defparameter *cursor-colors*
  '("royalblue"
    "sandybrown"
    "seagreen3"
    "violetred2"
    "aliceblue"
    "chocolate1"
    "chartreuse"
    "firebrick"
    "mediumorchid1"))

(defvar *server* (jsonrpc:make-server))
(defvar *clients* (make-hash-table :test 'equal))

(defstruct client
  id
  color
  connection)

(defun start (address)
  (jsonrpc:expose *server* "co/insert-string" 'api/insert-string)
  (jsonrpc:expose *server* "co/delete-string" 'api/delete-string)
  (jsonrpc:expose *server* "co/move-cursor" 'api/move-cursor)
  (jsonrpc:expose *server* "co/did-open" 'api/did-open)
  (uiop:delete-file-if-exists address)
  (jsonrpc:server-listen *server*
                         :mode :local-domain-socket
                         :address address))

(defun expose (method-name function)
  (jsonrpc:expose *server* method-name function))

(defun find-client (client-id)
  (gethash client-id *clients*))

(defun register-client (client-id)
  (or (gethash client-id *clients*)
      (setf (gethash client-id *clients*)
            (make-client :id client-id
                         :color (elt *cursor-colors*
                                     (mod (hash-table-count *clients*)
                                          (length *cursor-colors*)))
                         :connection jsonrpc/connection:*connection*))))

(defun broadcast (client method params)
  (dolist (conn (jsonrpc/server::server-client-connections *server*))
    (unless (eq conn (client-connection client))
      (jsonrpc:call-to *server* conn method params))))

(defun api/did-open (params)
  (with-hash-bindings (client-id
                       file-name
                       text)
      params
    (let ((client (register-client client-id))
          (document (find-document file-name)))
      (cond (document
             (add-client document client)
             (document-text document))
            (t
             (add-client (register-document file-name text)
                         client)
             nil)))))

(defun api/insert-string (params)
  (with-hash-bindings (client-id
                       file-name
                       line
                       character
                       string)
      params
    (alexandria:when-let* ((client (find-client client-id))
                           (document (find-document file-name)))
      (insert-string (document-buffer document) line character string)
      (broadcast client
                 "insert-string"
                 (hash "client" (hash "id" client-id
                                      "color" (client-color client))
                       "file-name" file-name
                       "line" line
                       "character" character
                       "string" string)))))

(defun api/delete-string (params)
  (with-hash-bindings (client-id
                       file-name
                       line
                       character
                       n)
      params
    (alexandria:when-let* ((client (find-client client-id))
                           (document (find-document file-name)))
      (delete-string (document-buffer document) line character n)
      (broadcast client
                 "delete-string"
                 (hash "client" (hash "id" client-id
                                      "color" (client-color client))
                       "file-name" file-name
                       "line" line
                       "character" character
                       "n" n)))))

(defun api/move-cursor (params)
  (with-hash-bindings (client-id
                       file-name
                       line
                       character)
      params
    (alexandria:when-let ((client (find-client client-id)))
      (jsonrpc:broadcast *server*
                         "move-cursor"
                         (hash "client" (hash "id" (client-id client)
                                              "color" (client-color client))
                               "file-name" file-name
                               "line" line
                               "character" character)))))

;;;
(defvar *documents* '())

(defclass document ()
  ((clients :initform '()
            :accessor document-clients)
   (file-name :initarg :file-name
              :reader document-file-name)
   (buffer :initarg :buffer
           :reader document-buffer)))

(defmethod document-text ((document document))
  (lem:buffer-text (document-buffer document)))

(defun find-document (file-name)
  (find file-name
        *documents*
        :key #'document-file-name
        :test #'string=))

(defun register-document (file-name text)
  (let ((buffer (lem:make-buffer file-name
                                 :temporary t
                                 :enable-undo-p nil)))
    (lem:insert-string (lem:buffer-point buffer) text)
    (let ((document (make-instance 'document
                                   :file-name file-name
                                   :buffer buffer)))
      (push document *documents*)
      document)))

(defmethod add-client ((document document) client)
  (push client (document-clients document)))

;; TODO:
;; - bufferを閉じたときにdocument-clientsから要素を取り除く
