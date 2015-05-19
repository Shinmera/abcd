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
   :op (error "OP required.")
   :system (error "SYSTEM required.")))

(defmethod asdf:operate (op (component delegate-op-component) &rest args)
  (apply #'asdf:operate
         (delegate-op-component-operation component)
         (delegate-op-component-system component)
         args))

(defgeneric c-system-includable-pathnames (system)
  (:method ((system c-system))
    (let ((includes system))
      (dolist (header (find-components 'c-header system :test #'typep))
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
      :libraries (,(asdf/system:component-build-pathname system)))))

(define-dependency-def-parser :shared-library (definition)
  definition)

(define-dependency-resolver :shared-library (system dependant &rest op-args &key)
  (let ((dependant (asdf:find-system dependant T)))
    (setf (component-effective-options system)
          (merge-options (component-effective-options system)
                         (c-system-library-options dependant)))
    (make-instance 'delegate-op-component
                   :system dependant
                   :op (apply #'make-instance 'link-op op-args))))

(define-dependency-def-parser :static-library (definition)
  definition)

(define-dependency-resolver :static-library (system dependant &rest op-args &key)
  (let ((dependant (asdf:find-system dependant T)))
    (setf (component-effective-options system)
          (merge-options (component-effective-options system)
                         (c-system-library-options dependant)))
    (make-instance 'delegate-op-component
                   :system dependant
                   :op (apply #'make-instance 'archive-op op-args))))
