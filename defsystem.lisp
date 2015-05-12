#|
 This file is a part of ABCD
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.abcd)

;; In order to allow more sensible names in ASDF component definitions we need
;; to hack into the class resolving mechanism. Since ASDF is (sadly) not
;; extensible by default in this regard, we have to override the function with
;; our own.

(defvar *type-name-map* (make-hash-table :test 'equal))
(defvar *standard-asdf-class-for-type* #'asdf::class-for-type)

(defun component-name-resolver (type-name)
  (gethash (string type-name) *type-name-map*))

(defun (setf component-name-resolver) (function type-name)
  (setf (gethash (string type-name) *type-name-map*) function))

(defun remove-component-name-resolver (type-name)
  (remhash (string type-name) *type-name-map*))

(defmacro define-component-name-resolver (name (parent) &body body)
  `(setf (component-name-resolver ,name)
         (lambda (,parent)
           ,@body)))

(defun asdf/parse-defsystem:class-for-type (parent type)
  (or (let ((func (component-name-resolver type)))
        (when func
          (funcall func parent)))
      (funcall *standard-asdf-class-for-type* parent type)))
