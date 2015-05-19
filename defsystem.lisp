#|
 This file is a part of ABCD
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.abcd)

;;; Component name resolving
;; In order to allow more sensible names in ASDF component definitions we need
;; to hack into the class resolving mechanism. Since ASDF is (sadly) not
;; extensible by default in this regard, we have to override the function with
;; our own. This will of course break horribly if ASDF happens to be upgraded
;; on the fly. We hope that nobody is crazy enough to do that.

(define-function-map-wrappers component-name-resolver)

(defmacro define-component-name-resolver (name (parent) &body body)
  `(setf (component-name-resolver ',name)
         (lambda (,parent)
           ,@body)))

(defvar *standard-asdf-class-for-type* #'asdf/parse-defsystem:class-for-type)
(defun asdf/parse-defsystem:class-for-type (parent type)
  (or (let ((func (component-name-resolver type)))
        (when func
          (funcall func parent)))
      (funcall *standard-asdf-class-for-type* parent type)))

;;; Dependency definition forms
;; In order to allow custom dependency forms in the ASDF system definitions we
;; need to hack into this. Again, ASDF is not extensible in this aspect so we
;; have to override the function with our own that is extensible.

(define-function-map-wrappers dependency-def-parser)

(defmacro define-dependency-def-parser (name (definition) &body body)
  `(setf (dependency-def-parser ',name)
         (lambda (,definition)
           ,@body)))

(defvar *standard-asdf-parse-dependency-def* #'asdf/parse-defsystem::parse-dependency-def)
(defun asdf/parse-defsystem::parse-dependency-def (definition)
  (or (when (listp definition)
        (let ((func (dependency-def-parser (car definition))))
          (when func
            (funcall func definition))))
      (funcall *standard-asdf-parse-dependency-def* definition)))

;;; Dependency form resolving
;; In this case ASDF is, curiously enough, actually extensible through methods.
;; No nasty hacks needed for this, but we will limit ourselves to a single method
;; with custom dispatch on a name table.

(define-function-map-wrappers dependency-resolver)

(defmacro define-dependency-resolver (name (system &rest args) &body body)
  `(setf (dependency-resolver ',name)
         (lambda (,system ,@args)
           ,@body)))

(defmethod asdf/find-component:resolve-dependency-combination ((system c-system) name args)
  (or (let ((func (dependency-resolver name)))
        (when func
          (apply func system args)))
      (call-next-method)))
