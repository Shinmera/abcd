#|
 This file is a part of ABCD
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.abcd)

(defvar *default-flags* '(:warnings :all))

(define-asdf/interface-class c-compiler-op (asdf:operation)
  ((direct-flags :initarg :flags :accessor operation-direct-flags)
   (effective-flags :initform *default-flags* :accessor operation-effective-flags)
   (compiler-function :initarg :compiler-function :accessor operation-compiler-function)
   (compiler :initarg :compiler :accessor operation-compiler))
  (:default-initargs
   :flags ()
   :compiler NIL
   :compiler-function #'c-compile))

(defmethod asdf/operation:operation-original-initargs ((op c-compiler-op))
  `(:flags ,(operation-direct-flags op)
    :compiler ,(operation-compiler op)))

(defmethod execute ((op c-compiler-op) inputs outputs)
  (loop for input in inputs
        for output in outputs
        do (apply (operation-compiler-function op)
                  (or (operation-compiler op) T)
                  (minimal-shell-namestring input)
                  (minimal-shell-namestring output)
                  (operation-effective-flags op))))

(define-asdf/interface-class compute-flags-op (asdf:upward-operation)
  ())

(defmethod asdf:operation-done-p ((op compute-flags-op) component)
  NIL)

(defmethod asdf:perform ((op compute-flags-op) component)
  NIL)

(define-asdf/interface-class preprocess-op (c-compiler-op asdf:selfward-operation asdf:sideway-operation)
  ((asdf:selfward-operation :initform 'compute-flags-op :allocation :class))
  (:default-initargs
   :compiler-function #'c-preprocess))

(defmethod asdf:perform ((op preprocess-op) component)
  NIL)

(define-asdf/interface-class assemble-op (c-compiler-op asdf:selfward-operation asdf:sideway-operation asdf:downward-operation)
  ((asdf:selfward-operation :initform '(compute-flags-op preprocess-op) :allocation :class))
  (:default-initargs
   :compiler-function #'c-assemble))

(defmethod asdf:perform ((op assemble-op) component)
  NIL)

(define-asdf/interface-class link-op (c-compiler-op asdf:selfward-operation asdf:downward-operation)
  ((asdf:selfward-operation :initform 'compute-flags-op :allocation :class)
   (asdf:downward-operation :initform 'assemble-op :allocation :class))
  (:default-initargs
   :compiler-function #'c-link))

(defmethod asdf:perform ((op link-op) component)
  NIL)

(defmethod execute ((op link-op) inputs outputs)
  (let ((output (first outputs)))
    (when (cdr outputs)
      (warn "Don't know how to use multiple outputs with ~a" op))
    (apply (operation-compiler-function op)
           (or (operation-compiler op) T)
           (mapcar #'minimal-shell-namestring inputs)
           (minimal-shell-namestring output)
           (operation-effective-flags op))))

(defmethod asdf:action-description ((op compute-flags-op) (c asdf:component))
  (format nil "~@<computing flags for ~3i~_~A~@:>" c))

(defmethod asdf:action-description ((op preprocess-op) (c asdf:component))
  (format nil "~@<preprocessing ~3i~_~A~@:>" c))

(defmethod asdf:action-description ((op assemble-op) (c asdf:component))
  (format nil "~@<assembling ~3i~_~A~@:>" c))

(defmethod asdf:action-description ((op link-op) (c asdf:component))
  (format nil "~@<linking ~3i~_~A~@:>" c))

#+:verbose
(defmethod asdf:perform :before ((op c-compiler-op) component)
  (v:trace :abcd.build "~a" (asdf:action-description op component)))
