(defpackage :lem-replica
  (:use :cl
        :lem-replica/utils
        :lem-replica/view)
  (:local-nicknames (:display :lem-core/display)
                    (:queue :lem/common/queue)
                    (:user :lem-replica/user)
                    (:permission :lem-replica/permission)
                    (:idle-time :lem-replica/idle-time))
  (:export :run-tcp-server
           :run-stdio-server
           :run-websocket-server
           :main))
(in-package :lem-replica)

(defparameter *version* "0.0.0")

(defvar *server-runner*)

(defclass server-runner ()
  ((http-port :initarg :http-port
              :reader server-runner-http-port)))

;;;
(defclass websocket-server-runner (server-runner)
  ((port :initarg :port
         :reader websocket-server-runner-port)
   (host :initarg :host
         :reader websocket-server-runner-host)))

(defmethod server-listen ((runner websocket-server-runner) server)
  (jsonrpc:server-listen server
                         :mode :websocket
                         :port (websocket-server-runner-port runner)
                         :host (websocket-server-runner-host runner)))

;;;
(defclass stdio-server-runner (server-runner)
  ())

(defmethod server-listen ((runner stdio-server-runner) server)
  (jsonrpc:server-listen server
                         :mode :stdio))

;;;
(defclass local-domain-socket-server-runner (server-runner)
  ((address :initarg :address :reader local-domain-socket-server-runner-address)))

(defmethod server-listen ((runner local-domain-socket-server-runner) server)
  (jsonrpc:server-listen server
                         :mode :local-domain-socket
                         :address (local-domain-socket-server-runner-address runner)))

;;;
(defun permitted-p (user permission-key)
  (let ((permission (user:user-permission user)))
    (ecase permission-key
      (:write (permission:permission-write permission))
      (:read (permission:permission-read permission)))))

(defclass server (jsonrpc:server) ())

(defclass jsonrpc (lem:implementation)
  ((server :initform (make-instance 'server)
           :reader jsonrpc-server)
   (display-width :initform 80
                  :accessor jsonrpc-display-width)
   (display-height :initform 24
                   :accessor jsonrpc-display-height)
   (background-color :accessor jsonrpc-background-color)
   (foreground-color :accessor jsonrpc-foreground-color)
   (message-queue :initform (queue:make-queue)
                  :reader jsonrpc-message-queue)
   (editor-thread :initform nil
                  :accessor jsonrpc-editor-thread))
  (:default-initargs
   :name :jsonrpc
   :redraw-after-modifying-floating-window t
   :window-left-margin 1))

(defun get-all-views ()
  (if (null (lem:current-frame))
      (vector)
      (coerce
       (loop :for window :in (append (lem:frame-header-windows (lem:current-frame))
                                     (lem:window-list)
                                     (lem:frame-floating-windows (lem:current-frame)))
             :collect (lem:window-view window))
       'vector)))

(defmethod jsonrpc/base:on-close-connection ((server server) connection)
  (pdebug "Removing ~A" connection)
  (with-error-handler ()
    (let ((user (user:get-user connection)))
      (notify (lem:implementation) "user-exit" (hash "userId" (user:user-id user))))
    (user:exit-user connection)))

(defmethod resize-display ((jsonrpc jsonrpc) width height)
  (setf (jsonrpc-display-width jsonrpc) width
        (jsonrpc-display-height jsonrpc) height))

(defmethod notify ((jsonrpc jsonrpc) method argument)
  (jsonrpc:broadcast (jsonrpc-server jsonrpc) method argument))

(defmethod notify* ((jsonrpc jsonrpc) method argument)
  (queue:enqueue (jsonrpc-message-queue jsonrpc)
                 (hash "method" method "argument" argument)))

(defmethod notify-all ((jsonrpc jsonrpc))
  (let ((argument (coerce (loop :until (queue:empty-p (jsonrpc-message-queue jsonrpc))
                                :collect (queue:dequeue (jsonrpc-message-queue jsonrpc)))
                          'vector)))
    (notify jsonrpc "bulk" argument)))

(defun handle-login (jsonrpc logged-in-callback params)
  (pdebug "ready: ~A ~A" jsonrpc/connection:*connection* (pretty-json params))
  (with-error-handler ()
    (let* ((size (gethash "size" params))
           (foreground (gethash "foreground" params))
           (background (gethash "background" params))
           (permission (gethash "permission" params))
           (permission-read (if permission (gethash "read" permission) t))
           (permission-write (if permission (gethash "write" permission) t))
           (permission-resize-display (if permission (gethash "resizeDisplay" permission) nil))
           (user-id (gethash "userId" params)))
      (user:add-user jsonrpc/connection:*connection*
                     :user-id user-id
                     :permission (permission:make-permission
                                  :read permission-read
                                  :write permission-write
                                  :resize-display permission-resize-display))
      (when (and permission-resize-display size)
        (let ((width (gethash "width" size))
              (height (gethash "height" size)))
          (resize-display jsonrpc width height)))
      (when (and background permission-write)
        (alexandria:when-let (color (lem:parse-color background))
          (setf (jsonrpc-background-color jsonrpc) color)))
      (when (and foreground permission-write)
        (alexandria:when-let (color (lem:parse-color foreground))
          (setf (jsonrpc-foreground-color jsonrpc) color)))
      (funcall logged-in-callback)

      (notify jsonrpc "user-enter" (hash "userId" user-id))

      (let ((response (hash "views" (with-error-handler () (get-all-views))
                            "foreground" (lem-core::foreground-color)
                            "background" (lem-core::background-color)
                            "size" (hash "width" (lem:display-width)
                                         "height" (lem:display-height)))))
        (pdebug "login response: ~A" (pretty-json response))
        response))))

(defun login (jsonrpc logged-in-callback)
  (lambda (params)
    (handle-login jsonrpc logged-in-callback params)))

(defun redraw (args)
  (pdebug "redraw: ~A" (pretty-json args))
  (with-error-handler ()
    (let ((size (and args (gethash "size" args))))
      (when size
        (let ((width (gethash "width" size))
              (height (gethash "height" size)))
          (resize-display (lem:implementation) width height)
          (notify (lem:implementation) "resize-display" size)))
      (lem:send-event (lambda ()
                        (lem-core::adjust-all-window-size)
                        (lem:redraw-display :force t))))))

(defun git-clone (args)
  (pdebug "git clone: ~A" (pretty-json args))
  (with-error-handler ()
    (let* ((url (gethash "url" args))
           (output (make-string-output-stream))
           (status (nth-value 2 (uiop:run-program `("git" "clone" ,url)
                                                  :directory (user-homedir-pathname)
                                                  :error-output output
                                                  :ignore-error-status t))))
      (unwind-protect
           (notify (lem:implementation)
                   "finish-git-clone"
                   (hash "url" url
                         "success" (eql status 0)
                         "errorMessage" (get-output-stream-string output)))
        (close output)))))

(defun find-git-directory (directory)
  (unless (uiop:pathname-equal "/" directory)
    (dolist (dir (uiop:subdirectories directory))
      (when (uiop:pathname-equal
             ".git/"
             (enough-namestring
              dir
              (uiop:pathname-parent-directory-pathname dir)))
        (return-from find-git-directory dir)))
    (find-git-directory (uiop:pathname-parent-directory-pathname directory))))

(defun extract-get-config-url (pathname)
  (values (alexandria:assoc-value (alexandria:assoc-value (cl-ini:parse-ini pathname)
                                                          :|REMOTE "ORIGIN"|)
                                  :url)))

(defun find-git-url (directory)
  (alexandria:when-let* ((git-directory (find-git-directory directory))
                         (url (extract-get-config-url (merge-pathnames "config" git-directory))))
    url))

(defun extract-username-and-repository (url)
  (or (ppcre:register-groups-bind (username repository)
          ("git@github.com:(.+?)/(.+?)(?:\\.git)?$" url)
        (hash "username" username
              "repository" repository))
      (ppcre:register-groups-bind (username repository)
          ("https://github.com/(.+?)/(.+?)(?:\\.git)?$" url)
        (hash "username" username
              "repository" repository))))

(defun get-current-directory-info (args)
  (declare (ignore args))
  (with-error-handler ()
    (let* ((directory (lem:buffer-directory (lem:current-buffer)))
           (url (find-git-url directory)))
      (hash "directory" directory
            "git" (when url
                    (extract-username-and-repository url))))))

(defmethod lem-if:invoke ((jsonrpc jsonrpc) function)
  (when (uiop:getenv "MICROS_PORT")
    (micros:create-server :dont-close t
                          :port (parse-integer (uiop:getenv "MICROS_PORT"))
                          :interface "0.0.0.0"))

  (idle-time:update-last-input-time)
  (let ((ready nil))
    (setf (jsonrpc-editor-thread jsonrpc)
          (funcall function
                   (lambda ()
                     (loop :until ready)
                     (notify jsonrpc "startup" nil))))
    (jsonrpc:expose (jsonrpc-server jsonrpc)
                    "login"
                    (login jsonrpc
                           (lambda ()
                             (setf ready t))))
    (jsonrpc:expose (jsonrpc-server jsonrpc)
                    "input"
                    (lambda (args)
                      (input-callback jsonrpc args)))
    (jsonrpc:expose (jsonrpc-server jsonrpc)
                    "redraw"
                    'redraw)
    (jsonrpc:expose (jsonrpc-server jsonrpc)
                    "git-clone"
                    'git-clone)
    (jsonrpc:expose (jsonrpc-server jsonrpc)
                    "get-current-directory-info"
                    'get-current-directory-info)

    (lem:add-hook lem:*exit-editor-hook*
                  (lambda ()
                    (notify jsonrpc "exit" nil)
                    (uiop:quit 0)))

    (when (server-runner-http-port *server-runner*)
      (run-http-server (server-runner-http-port *server-runner*)))

    (server-listen *server-runner* (jsonrpc-server jsonrpc))))

(defmethod lem-if:get-background-color ((jsonrpc jsonrpc))
  (jsonrpc-background-color jsonrpc))

(defmethod lem-if:get-foreground-color ((jsonrpc jsonrpc))
  (jsonrpc-foreground-color jsonrpc))

(defmethod lem-if:update-foreground ((jsonrpc jsonrpc) color-name)
  (with-error-handler ()
    (notify jsonrpc "update-foreground" color-name)))

(defmethod lem-if:update-background ((jsonrpc jsonrpc) color-name)
  (with-error-handler ()
    (notify jsonrpc "update-background" color-name)))

(defmethod lem-if:update-cursor-shape ((jsonrpc jsonrpc) cursor-type)
  ;; TODO
  )

(defmethod lem-if:display-width ((jsonrpc jsonrpc))
  (with-error-handler ()
    (jsonrpc-display-width jsonrpc)))

(defmethod lem-if:display-height ((jsonrpc jsonrpc))
  (with-error-handler ()
    (jsonrpc-display-height jsonrpc)))

(defmethod lem-if:display-title ((jsonrpc jsonrpc))
  ;; TODO
  )

(defmethod lem-if:set-display-title ((jsonrpc jsonrpc) title)
  ;; TODO
  )

(defmethod lem-if:display-fullscreen-p ((jsonrpc jsonrpc))
  ;; TODO
  )

(defmethod lem-if:set-display-fullscreen-p ((jsonrpc jsonrpc) fullscreen-p)
  ;; TODO
  )

(defmethod lem-if:make-view ((jsonrpc jsonrpc) window x y width height use-modeline)
  (let ((view (make-view :window window
                         :x x
                         :y y
                         :width width
                         :height height
                         :use-modeline use-modeline
                         :kind (if (lem:floating-window-p window)
                                   "floating"
                                   "tile"))))
    (notify jsonrpc "make-view" view)
    view))

(defmethod lem-if:view-width ((jsonrpc jsonrpc) view)
  (view-width view))

(defmethod lem-if:view-height ((jsonrpc jsonrpc) view)
  (view-height view))

(defmethod lem-if:delete-view ((jsonrpc jsonrpc) view)
  (with-error-handler ()
    (notify jsonrpc "delete-view" (hash "viewInfo" view))))

(defmethod lem-if:clear ((jsonrpc jsonrpc) view)
  (with-error-handler ()
    (notify* jsonrpc "clear" (hash "viewInfo" view))))

(defmethod lem-if:set-view-size ((jsonrpc jsonrpc) view width height)
  (with-error-handler ()
    (resize-view view width height)
    (notify jsonrpc
            "resize-view"
            (hash "viewInfo" view
                  "width" width
                  "height" height))))

(defmethod lem-if:set-view-pos ((jsonrpc jsonrpc) view x y)
  (with-error-handler ()
    (move-view view x y)
    (notify jsonrpc
            "move-view"
            (hash "viewInfo" view
                  "x" x
                  "y" y))))

(defmethod lem-if:redraw-view-before ((jsonrpc jsonrpc) view)
  )

(defmethod lem-if:redraw-view-after ((jsonrpc jsonrpc) view)
  )

(defmethod lem-if:will-update-display ((jsonrpc jsonrpc))
  )

(defmethod lem-if:update-display ((jsonrpc jsonrpc))
  (with-error-handler ()
    (let ((view (lem:window-view (lem:current-window)))
          (x (lem:last-print-cursor-x (lem:current-window)))
          (y (lem:last-print-cursor-y (lem:current-window))))
      (notify* jsonrpc
               "move-cursor"
               (hash "viewInfo" view "x" x "y" y)))
    (notify* jsonrpc "update-display" nil)
    (notify-all jsonrpc)))

(defmethod lem-if:clipboard-paste ((jsonrpc jsonrpc))
  ;; TODO
  )

(defmethod lem-if:clipboard-copy ((jsonrpc jsonrpc) text)
  ;; TODO
  )

(defmethod lem-if:increase-font-size ((jsonrpc jsonrpc))
  ;; TODO
  )
(defmethod lem-if:decrease-font-size ((jsonrpc jsonrpc))
  ;; TODO
  )

(defmethod lem-if:resize-display-before ((jsonrpc jsonrpc))
  )

(defmethod lem-if:get-font-list ((jsonrpc jsonrpc))
  )

(defmethod lem-if:get-mouse-position ((jsonrpc jsonrpc))
  (values 0 0))

(defmethod lem-if:get-char-width ((jsonrpc jsonrpc))
  ;; TODO
  1)
(defmethod lem-if:get-char-height ((jsonrpc jsonrpc))
  ;; TODO
  1)

;;;;
(defun bool (x) (if x 'yason:true 'yason:false))

(defun ensure-rgb (color)
  (if (typep color 'lem:color)
      (lem:color-to-hex-string color)
      color))

(defmethod yason:encode ((attribute lem:attribute) &optional (stream *standard-output*))
  (with-error-handler ()
    (yason:with-output (stream)
      (yason:with-object ()
        (yason:encode-object-element "foreground" (ensure-rgb (lem:attribute-foreground attribute)))
        (yason:encode-object-element "background" (ensure-rgb (lem:attribute-background attribute)))
        (yason:encode-object-element "reverse" (bool (lem:attribute-reverse attribute)))
        (yason:encode-object-element "bold" (bool (lem:attribute-bold attribute)))
        (yason:encode-object-element "underline" (bool (lem:attribute-underline attribute)))))))

(defmethod yason:encode ((view view) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "id" (view-id view))
      (yason:encode-object-element "x" (view-x view))
      (yason:encode-object-element "y" (view-y view))
      (yason:encode-object-element "width" (view-width view))
      (yason:encode-object-element "height" (view-height view))
      (yason:encode-object-element "use_modeline" (view-use-modeline view))
      (yason:encode-object-element "kind" (view-kind view)))))




;;; drawing
(defgeneric object-width (drawing-object))

(defmethod object-width ((drawing-object display:void-object))
  0)

(defmethod object-width ((drawing-object display:text-object))
  (lem-core:string-width (display:text-object-string drawing-object)))

(defmethod object-width ((drawing-object display:eol-cursor-object))
  0)

(defmethod object-width ((drawing-object display:extend-to-eol-object))
  0)

(defmethod object-width ((drawing-object display:line-end-object))
  0)

(defmethod object-width ((drawing-object display:image-object))
  0)

(defgeneric draw-object (jsonrpc object x y view))

(defmethod draw-object (jsonrpc (object display:void-object) x y view)
  (values))

(defvar *put-target* :edit-area)

(defun put (jsonrpc view x y string attribute)
  (with-error-handler ()
    (notify* jsonrpc
             (ecase *put-target*
               (:edit-area "put")
               (:modeline "modeline-put"))
             (hash "viewInfo" view
                   "x" x
                   "y" y
                   "text" string
                   "textWidth" (lem:string-width string)
                   "attribute" (lem:ensure-attribute attribute nil)))))

(defmethod draw-object (jsonrpc (object display:text-object) x y view)
  (let ((string (display:text-object-string object))
        (attribute (display:text-object-attribute object)))
    (when (and attribute (lem-core:cursor-attribute-p attribute))
      (lem-core::set-last-print-cursor (view-window view) x y))
    (put jsonrpc view x y string attribute)))

(defmethod draw-object (jsonrpc (object display:eol-cursor-object) x y view)
  (lem-core::set-last-print-cursor (view-window view) x y)
  (put jsonrpc view x y " "
       (lem:make-attribute
        :background
        (lem:color-to-hex-string (display:eol-cursor-object-color object)))))

(defmethod draw-object (jsonrpc (object display:extend-to-eol-object) x y view)
  (let ((width (lem-if:view-width (lem-core:implementation) view)))
    (when (< x width)
      (put jsonrpc view x y
           (make-string (- width x) :initial-element #\space)
           (lem:make-attribute
            :background
            (lem:color-to-hex-string (display:extend-to-eol-object-color object)))))))

(defmethod draw-object (jsonrpc (object display:line-end-object) x y view)
  (let ((string (display:text-object-string object))
        (attribute (display:text-object-attribute object)))
    (put jsonrpc
         view
         (+ x (display:line-end-object-offset object))
         y
         string
         attribute)))

(defmethod draw-object (jsonrpc (object display:image-object) x y view)
  (values))

(defun render-line (jsonrpc view x y objects)
  (loop :for object :in objects
        :do (draw-object jsonrpc object x y view)
            (incf x (object-width object))))

(defun render-line-from-behind (jsonrpc view y objects)
  (loop :with current-x := (view-width view)
        :for object :in objects
        :do (decf current-x (object-width object))
            (draw-object jsonrpc object current-x y view)))

(defmethod lem-if:render-line ((jsonrpc jsonrpc) view x y objects height)
  (with-error-handler ()
    (notify* jsonrpc
             "clear-eol"
             (hash "viewInfo" view
                   "x" x
                   "y" y))
    (render-line jsonrpc view x y objects)))

(defmethod lem-if:render-line-on-modeline ((jsonrpc jsonrpc) view left-objects right-objects
                                           default-attribute height)
  (let ((*put-target* :modeline))
    (with-error-handler ()
      (notify* jsonrpc
               "modeline-put"
               (hash "viewInfo" view
                     "x" 0
                     "y" 0
                     "text" (make-string (view-width view) :initial-element #\space)
                     "textWidth" (view-width view)
                     "attribute" default-attribute))
      (render-line jsonrpc view 0 0 left-objects)
      (render-line-from-behind jsonrpc view 0 right-objects))))

(defmethod lem-if:object-width ((jsonrpc jsonrpc) drawing-object)
  (object-width drawing-object))

(defmethod lem-if:object-height ((jsonrpc jsonrpc) drawing-object)
  1)

(defmethod lem-if:clear-to-end-of-window ((jsonrpc jsonrpc) view y)
  (notify* jsonrpc
           "clear-eob"
           (hash "viewInfo" view
                 "x" 0
                 "y" y)))


;;;
(defconstant +abort+ 0)
(defconstant +keyevent+ 1)
(defconstant +resize+ 2)

(defun convert-keyevent (e)
  (let ((key (gethash "key" e))
        (ctrl (gethash "ctrl" e))
        (meta (gethash "meta" e))
        (super (gethash "super" e))
        (shift (gethash "shift" e)))
    (cond ((string= key " ") (setf key "Space")))
    (lem:make-key :ctrl ctrl
                  :meta meta
                  :super super
                  :shift (if (lem:insertion-key-sym-p key) nil shift)
                  :sym key)))

(defun input-callback (jsonrpc args)
 (handler-case
      (let ((user (user:get-user jsonrpc/connection:*connection*)))
        (when (permitted-p user :write)
          (idle-time:update-last-input-time)
          (let ((kind (gethash "kind" args))
                (value (gethash "value" args)))
            (cond ((= kind +abort+)
                   (lem:send-abort-event (jsonrpc-editor-thread jsonrpc) nil))
                  ((= kind +keyevent+)
                   (when value
                     (notify jsonrpc
                             "user-input"
                             (hash "userId" (user:user-id user)
                                   "value" value))
                     (let ((key (convert-keyevent value)))
                       (lem:send-event key))))
                  ((= kind +resize+)
                   (resize-display jsonrpc
                                   (gethash "width" value)
                                   (gethash "height" value))
                   (lem:send-event :resize))
                  (t
                   (error "unexpected kind: ~D" kind))))))
    (error (e)
      (pdebug "input-callback: ~A ~A" e
              (with-output-to-string (stream)
                (let ((stream (yason:make-json-output-stream stream)))
                  (yason:encode args stream)))))))

;;;
(defvar *app* (make-instance 'ningle:app))

(setf (ningle:route *app* "/ping" :method :GET) 'ping
      (ningle:route *app* "/users" :method :GET) 'get-users
      (ningle:route *app* "/idle-time" :method :GET) 'get-idle-time)

(defun ping (params)
  (declare (ignore params))
  "pong")

(defmethod com.inuoe.jzon:coerced-fields ((user user:user))
  `(("id" ,(user:user-id user))))

(defun get-users (params)
  (declare (ignore params))
  `(200 (:content-type "application/json")
        ,(babel:string-to-octets
          (com.inuoe.jzon:stringify
           (coerce (user:get-users) 'vector)))))

(defun get-idle-time (params)
  (declare (ignore params))
  (princ-to-string (idle-time:idle-time)))

(defun run-http-server (port)
  (clack:clackup *app* :port port :address "0.0.0.0"))

;;;
(defparameter +command-line-spec+
  '(("mode" :type string :optional t :documentation "\"websocket\", \"stdio\", \"local-domain-socket\"")
    ("port" :type integer :optional nil :documentation "port of \"websocket\"")
    ("host" :type string :optional t)
    ("http-port" :type integer :optional nil)
    ("address" :type string :optional t :documentation "address of \"local-domain-socket\"")
    ("co-editing-address" :type string :optional t)))

(defun run-websocket-server (&key (port 50000) (hostname "127.0.0.1") (http-port 50001))
  (let ((*server-runner*
          (make-instance 'websocket-server-runner
                         :port port
                         :host hostname
                         :http-port http-port)))
    (lem:lem)))

(defun run-stdio-server (&key (http-port 50001))
  (let ((*server-runner* (make-instance 'stdio-server-runner
                                        :http-port http-port)))
    (lem:lem)))

(defun run-local-domain-socket-server (&key address (http-port 50001))
  (let ((*server-runner* (make-instance 'local-domain-socket-server-runner
                                        :address address
                                        :http-port http-port)))
    (lem:lem)))

(defun check-port-specified (port)
  (unless port
    (command-line-arguments:show-option-help +command-line-spec+)
    (uiop:quit 1)))

(defun main (&optional (args (uiop:command-line-arguments)))
  (command-line-arguments:handle-command-line
   +command-line-spec+
   (lambda (&key (mode "websocket")
                 port
                 (host "127.0.0.1")
                 address
                 http-port
                 co-editing-address)
     (when co-editing-address
       (lem-co-editing/client::start-sync co-editing-address))
     (cond ((string= mode "websocket")
            (check-port-specified port)
            (run-websocket-server :port port :hostname host :http-port http-port))
           ((string= mode "stdio")
            (run-stdio-server :http-port http-port))
           ((string= mode "local-domain-socket")
            (run-local-domain-socket-server :address address :http-port http-port))
           (t
            (command-line-arguments:show-option-help +command-line-spec+)
            (uiop:quit 1))))
   :name "lem-replica"
   :positional-arity 0
   :command-line args))
