(defpackage :lem-replica/idle-time
  (:use :cl)
  (:export :update-last-input-time
           :idle-time))
(in-package :lem-replica/idle-time)

(defvar *last-input-time* nil)

(defun update-last-input-time ()
  (setf *last-input-time* (get-universal-time)))

(defun idle-time ()
  (if *last-input-time*
      (- (get-universal-time) *last-input-time*)
      0))
