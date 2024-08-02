(defpackage :lem-co-editing/client
  (:use :cl :lem-co-editing/utils))
(in-package :lem-co-editing/client)

(defvar *inhibit-handle* nil)
(defvar *client*)

(defclass client (jsonrpc:client)
  ((id :initform (random-uuid)
       :reader client-id)))

(defun create-client (address)
  (let ((client (make-instance 'client)))
    (jsonrpc:client-connect client
                            :mode :local-domain-socket
                            :address address)
    client))

(defun start-sync (address)
  (setf *client* (create-client address))

  (jsonrpc:expose *client* "insert-string" 'api/insert-string)
  (jsonrpc:expose *client* "delete-string" 'api/delete-string)
  (jsonrpc:expose *client* "update-text" 'api/update-text)
  (jsonrpc:expose *client* "move-cursor" 'api/move-cursor)

  (lem:add-hook lem:*find-file-hook* 'find-file-hook)
  (lem:add-hook lem:*post-command-hook* 'move-cursor)
  (lem:add-hook (lem:variable-value 'lem:before-change-functions :global t) 'sync-buffer)

  (set-sync-buffer (lem:current-buffer)))

(defun sync-buffer-p (buffer)
  (lem:buffer-value buffer 'sync-buffer))

(defun set-sync-buffer (buffer)
  (setf (lem:buffer-value buffer 'sync-buffer) t))

(defun find-file-hook (buffer)
  (when (lem:buffer-filename buffer)
    (set-sync-buffer buffer)
    (let ((text (did-open buffer)))
      (when text
        (let ((*inhibit-handle* t))
          (replace-text buffer text))))))

(defun sync-buffer (point arg)
  (when (sync-buffer-p (lem:point-buffer point))
    (unless *inhibit-handle*
      (let ((buffer (lem:point-buffer point)))
        (cond ((characterp arg)
               (send-write-string (lem:buffer-filename buffer)
                                  (lem:line-number-at-point point)
                                  (lem:point-charpos point)
                                  (string arg)))
              ((stringp arg)
               (send-write-string (lem:buffer-filename buffer)
                                  (lem:line-number-at-point point)
                                  (lem:point-charpos point)
                                  arg))
              ((integerp arg)
               (send-delete-string (lem:buffer-filename buffer)
                                   (lem:line-number-at-point point)
                                   (lem:point-charpos point)
                                   arg)))))))

(defun move-cursor ()
  (send-cursor (lem:current-point)))

;;;
(defun buffer-user-cursors (buffer)
  (or (lem:buffer-value buffer 'cursors)
      (setf (lem:buffer-value buffer 'cursors)
            (make-hash-table :test 'equal))))

(defun get-user-cursor (buffer client-id)
  (gethash client-id (buffer-user-cursors buffer)))

(defun set-user-cursor (buffer client-id client-color line character)
  (let ((cursor (get-user-cursor buffer client-id)))
    (unless cursor
      (setf cursor
            (make-cursor (lem:buffer-point buffer)
                         client-id
                         client-color))
      (setf (gethash client-id (buffer-user-cursors buffer)) cursor))
    (move cursor line character)
    cursor))

(defun move-other-user-cursor (client-id client-color buffer line character)
  (set-user-cursor buffer client-id client-color line character))

(defun api/insert-string (params)
  (do-log "insert-string: ~A" (pretty-json params))
  (with-hash-bindings (client
                       file-name
                       line
                       character
                       string)
      params
    (let ((client-id (gethash "id" client))
          (client-color (gethash "color" client)))
      (unless (equal client-id (client-id *client*))
        (lem:send-event
         (lambda ()
           (let ((*inhibit-handle* t))
             (alexandria:when-let ((buffer (lem:get-file-buffer file-name)))
               (move-other-user-cursor client-id client-color buffer line character)
               (insert-string buffer line character string)))
           (lem:redraw-display)))))))

(defun api/delete-string (params)
  (do-log "delete-string: ~A" (pretty-json params))
  (with-hash-bindings (client
                       file-name
                       line
                       character
                       n)
      params
    (let ((client-id (gethash "id" client))
          (client-color (gethash "color" client)))
      (unless (equal client-id (client-id *client*))
        (lem:send-event
         (lambda ()
           (let ((*inhibit-handle* t))
             (alexandria:when-let ((buffer (lem:get-file-buffer file-name)))
               (move-other-user-cursor client-id client-color buffer line character)
               (delete-string buffer line character n)))
           (lem:redraw-display)))))))

(defun api/update-text (params)
  (do-log "update-text: ~A" (pretty-json params))
  (with-hash-bindings (client
                       file-name
                       text)
      params
    (declare (ignore client))
    (lem:send-event
     (lambda ()
       (alexandria:when-let (buffer (lem:get-file-buffer file-name))
         (let ((*inhibit-handle* t))
           (replace-text buffer text)
           (lem:redraw-display)))))))

(defun api/move-cursor (params)
  (do-log "move-cursor: ~A" (pretty-json params))
  (with-hash-bindings (client
                       file-name
                       line
                       character)
      params
    (let ((client-id (gethash "id" client))
          (client-color (gethash "color" client)))
      (unless (equal client-id (client-id *client*))
        (lem:send-event
         (lambda ()
           (let ((*inhibit-handle* t))
             (alexandria:when-let (buffer (lem:get-file-buffer file-name))
               (move-other-user-cursor client-id client-color buffer line character)))
           (lem:redraw-display)))))))

;;;
(defvar *logger* nil)

(defun do-log (string &rest args)
  (when *logger*
    (apply #'format *logger* string args)))

;;;
(defun did-open (buffer)
  (jsonrpc:call *client*
                "co/did-open"
                (hash "client-id" (client-id *client*)
                      "file-name" (lem:buffer-filename buffer)
                      "text" (lem:buffer-text buffer))))

(defun send-write-string (file-name line character string)
  (jsonrpc:call *client*
                "co/insert-string"
                (hash "client-id" (client-id *client*)
                      "file-name" file-name
                      "line" line
                      "character" character
                      "string" string)))

(defun send-delete-string (file-name line character n)
  (jsonrpc:call *client*
                "co/delete-string"
                (hash "client-id" (client-id *client*)
                      "file-name" file-name
                      "line" line
                      "character" character
                      "n" n)))

(defun send-cursor (point)
  (alexandria:when-let ((file-name (lem:buffer-filename (lem:point-buffer point))))
    (let ((line (lem:line-number-at-point point))
          (character (lem:point-charpos point)))
      (jsonrpc:notify *client*
                      "co/move-cursor"
                      (hash "client-id" (client-id *client*)
                            "file-name" file-name
                            "line" line
                            "character" character)))))

;;;
(defclass cursor ()
  ((overlay :initarg :overlay :reader cursor-overlay)
   (client-id :initarg :client-id)))

(defclass other-user-cursor-overlay (lem-core::cursor-overlay)
  ())

(defun make-cursor (point client-id client-color)
  (let ((point (lem:copy-point point :temporary)))
    (make-instance 'cursor
                   :client-id client-id
                   :overlay (make-instance 'other-user-cursor-overlay
                                           :start point
                                           :end point
                                           :buffer (lem:point-buffer point)
                                           :temporary nil
                                           :fake nil
                                           :attribute (lem:make-attribute :background client-color)))))

(defmethod move ((cursor cursor) line character)
  (let ((start (lem:overlay-start (cursor-overlay cursor)))
        (end (lem:overlay-end (cursor-overlay cursor))))
    (lem:move-point end (move-point start line character))))
