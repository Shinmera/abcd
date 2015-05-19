#|
 This file is a part of ABCD
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.abcd)

(defclass delegate-op-component (asdf:component)
  ((operation :initarg :op :accessor delegate-op-component-operation)
   (system :initarg :system :accessor delegate-op-component-system))
  (:default-initargs
   :name "DELEGATE"
   :op (error "OP required.")
   :system (error "SYSTEM required.")))

(defmethod print-object ((component delegate-op-component) stream)
  (print-unreadable-object (component stream :type T)
    (format stream ":operation ~s :system ~s"
            (delegate-op-component-operation component)
            (delegate-op-component-system component))))

(defmethod asdf:needed-in-image-p (op (component delegate-op-component))
  (asdf:needed-in-image-p
   (delegate-op-component-operation component)
   (delegate-op-component-system component)))

(defmethod asdf:operate (op (component delegate-op-component) &rest args)
  (apply #'asdf:operate
         (delegate-op-component-operation component)
         (delegate-op-component-system component)
         args))

(defgeneric c-system-includable-pathnames (system)
  (:method ((system c-system))
    (let ((includes ()))
      (dolist (header (find-components 'c-header system :test #'typep))
        (asdf:operate 'compute-options-op header)
        (when (getf (component-effective-options header) :includable)
          (pushnew (uiop:pathname-directory-pathname
                    (asdf:component-pathname header))
                   includes :test #'equal)))
      includes)))

(defgeneric c-system-library-options (system)
  (:method ((system c-system))
    (assert (or (c-system-shared-library system)
                (c-system-static-library system)))
    `(:include-dirs ,(c-system-includable-pathnames system)
      :library-dirs (,(uiop:pathname-directory-pathname
                       (asdf/system:component-build-pathname system)))
      :libraries (,(pathname-name (asdf/system:component-build-pathname system))))))

(define-dependency-def-parser :shared-library (definition)
  definition)

(define-dependency-resolver :shared-library (system dependant &key)
  (let ((dependant (asdf:find-system dependant T)))
    (setf (component-direct-options system)
          (merge-options (component-direct-options system)
                         (c-system-library-options dependant)))
    dependant))

(define-dependency-def-parser :static-library (definition)
  definition)

(define-dependency-resolver :static-library (system dependant &key)
  (let ((dependant (asdf:find-system dependant T)))
    (setf (component-direct-options system)
          (merge-options (component-direct-options system)
                         (c-system-library-options dependant)))
    dependant))
