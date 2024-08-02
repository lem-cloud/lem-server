(defpackage :lem-replica/permission
  (:use :cl)
  (:export :permission
           :make-permission
           :permission-read
           :permission-write
           :permission-resize-display))
(in-package :lem-replica/permission)

(defclass permission ()
  ((read :initarg :read
         :reader permission-read
         :type boolean)
   (write :initarg :write
          :reader permission-write
          :type boolean)
   (resize-display :initarg :resize-display
                   :reader permission-resize-display
                   :type boolean)))

(defun make-permission (&key read write resize-display)
  (make-instance 'permission
                 :read read
                 :write write
                 :resize-display resize-display))
