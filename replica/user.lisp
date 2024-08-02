(defpackage :lem-replica/user
  (:use :cl)
  (:export :user
           :user-id
           :user-permission
           :get-users
           :add-user
           :get-user
           :exit-user))
(in-package :lem-replica/user)

(defclass user ()
  ((id :initarg :id
       :reader user-id)
   (permission :initarg :permission
               :reader user-permission
               :type permission)
   (connection :initarg :connection
               :reader user-connection)))

(defvar *user-table* '())
(defvar *user-table-mutex* (bt2:make-lock :name "user-table-mutex"))

(defun get-users ()
  *user-table*)

(defun add-user (connection &key user-id permission)
  (bt2:with-lock-held (*user-table-mutex*)
    (setf *user-table*
          (cons (make-instance 'user
                               :connection connection
                               :id user-id
                               :permission permission)
                (delete user-id *user-table* :test #'equal :key #'user-id)))))

(defun get-user (connection)
  (bt2:with-lock-held (*user-table-mutex*)
    (find connection *user-table* :key #'user-connection :test #'eq)))

(defun exit-user (connection)
  (bt2:with-lock-held (*user-table-mutex*)
    (alexandria:deletef *user-table* connection :key #'user-connection :test #'eq)))
