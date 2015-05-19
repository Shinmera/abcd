#|
 This file is a part of ABCD
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.abcd)

(defclass application ()
  ((executable :initarg :executable :reader executable)))

(defgeneric invoke (application args &key output error-output &allow-other-keys)
  (:method ((application symbol) args &rest kargs)
    (apply #'invoke (make-instance (uiop:coerce-class application :package :org.shirakumo.abcd :super 'application)) args kargs))
  (:method ((application application) args &rest kargs &key (output T) (error-output T))
    (let ((command (with-clear-environment
                       (etypecase args
                         (list (list* (executable application) (mapcar #'externalize args)))
                         (string (format NIL "~a ~a" (executable application) args))))))
      #+:verbose (v:trace :abcd.compiler "Invoking ~a" command)
      (apply #'uiop:run-program command :output output :error-output error-output kargs))))

(defclass archiver (application)
  ()
  (:default-initargs :executable "ar"))
