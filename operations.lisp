#|
 This file is a part of ABCD
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.abcd)

(defvar *default-flags* '(:errors :all))

(define-asdf/interface-class c-compiler-op (asdf:operation)
  ((direct-flags :initarg :flags :accessor operation-direct-flags)
   (effective-flags :initform *default-flags* :accessor operation-effective-flags)
   (compiler-function :initarg :compiler-function :accessor operation-compiler-function))
  (:default-initargs
   :flags ()
   :compiler-arguments ()
   :compiler-function #'c-compile))

(defvar *level* 0)

(defmethod asdf:perform :around ((op c-compiler-op) (c asdf:component))
  (let ((*level* (1+ *level*)))
    (format T "~&[~d] ~a~%" *level* (asdf:action-description op c))
    (call-next-method)))

(defmethod execute ((op c-compiler-op) inputs outputs)
  (loop for input in inputs
        for output in outputs
        do (apply (operation-compiler-function op) T input output
                  (merge-flags (operation-direct-flags op)
                               (operation-effective-flags op)))))

(define-asdf/interface-class preprocess-op (c-compiler-op asdf:sideway-operation)
  ((asdf:sideway-operation :initform NIL :allocation :class))
  (:default-initargs
   :compiler-function #'c-preprocess))

(defmethod asdf:perform ((op preprocess-op) component)
  NIL)

(define-asdf/interface-class assemble-op (c-compiler-op asdf:selfward-operation asdf:sideway-operation asdf:downward-operation)
  ((asdf:selfward-operation :initform 'preprocess-op :allocation :class)
   (asdf:sideway-operation :initform NIL :allocation :class)
   (asdf:downward-operation :initform NIL :allocation :class))
  (:default-initargs
   :compiler-function #'c-assemble))

(defmethod asdf:perform ((op assemble-op) component)
  NIL)

(define-asdf/interface-class link-op (c-compiler-op asdf:downward-operation)
  ((asdf:downward-operation :initform 'assemble-op :allocation :class))
  (:default-initargs
   :compiler-function #'c-link))

(defmethod asdf:perform ((op link-op) component)
  NIL)

(defmethod execute ((op link-op) inputs outputs)
  (let ((output (first outputs)))
    (when (cdr outputs)
      (warn "Don't know how to use multiple outputs with ~a" op))
    (apply (operation-compiler-function op) T inputs output
           (merge-flags (operation-direct-flags op)
                        (operation-effective-flags op)))))

(defun preprocess-system (system &rest args &key flags force force-not verbose version &allow-other-keys)
  (declare (ignore force force-not verbose version))
  (apply 'asdf:operate (make-instance 'preprocess-op :flags flags) system args)
  T)

(defun assemble-system (system &rest args &key flags force force-not verbose version &allow-other-keys)
  (declare (ignore force force-not verbose version))
  (apply 'asdf:operate (make-instance 'assemble-op :flags flags) system args)
  T)

(defun link-system (system &rest args &key flags force force-not verbose version &allow-other-keys)
  (declare (ignore force force-not verbose version))
  (apply 'asdf:operate (make-instance 'link-op :flags flags) system args)
  T)

(defmethod asdf:action-description ((op preprocess-op) (c asdf:component))
  (format nil "~@<preprocessing ~3i~_~A~@:>" c))

(defmethod asdf:action-description ((op assemble-op) (c asdf:component))
  (format nil "~@<assembling ~3i~_~A~@:>" c))

(defmethod asdf:action-description ((op link-op) (c asdf:component))
  (format nil "~@<linking ~3i~_~A~@:>" c))
