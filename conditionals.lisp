#|
 This file is a part of ABCD
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.abcd)

(define-asdf/interface-class conditional-component (option-component asdf:module)
  ((test :initarg :test :initform #'test-conditional-fields :accessor conditional-test)
   (option :initarg :option :initform NIL :accessor conditional-option)
   (flag :initarg :flag :initform NIL :accessor conditional-flag)
   (feature :initarg :feature :initform NIL :accessor conditional-feature)))

(defun ensure-conditional-function (thing)
  (typecase thing
    (cons (cond ((eql (car thing) 'lambda)
                 (compile NIL thing))
                (T (compile NIL `(lambda (*) (declare (ignorable *)) (progn ,thing))))))
    (function thing)
    (T (constantly thing))))

(defun initialize-conditional-component (component &key test &allow-other-keys)
  (when test
    (setf (conditional-test component)
          (ensure-conditional-function test))))

(defmethod initialize-instance :after ((c conditional-component) &rest args &key)
  (apply #'initialize-conditional-component c args))

(defmethod reinitialize-instance :after ((c conditional-component) &rest args &key)
  (apply #'initialize-conditional-component c args))

(defmethod asdf/plan:needed-in-image-p (op (c conditional-component))
  NIL)

(defgeneric test-condition (conditional)
  (:method ((c conditional-component))
    (funcall (conditional-test c) c)))

(defmethod asdf:perform :around (op (c conditional-component))
  (cond ((test-condition c)
         #+:verbose (v:trace :abcd.build "Test succeeded, ~a"
                             (asdf:action-description op c))
         (call-next-method))
        (T
         #+:verbose (v:trace :abcd.build "Test failed, not ~a"
                             (asdf:action-description op c)))))

(defmethod asdf:component-children ((c conditional-component))
  (when (test-condition c)
    (call-next-method)))

(defun test-conditional-fields (component)
  (or (and (conditional-option component)
           (getf (component-effective-options component)
                 (conditional-option component)))
      (and (conditional-flag component)
           (find (conditional-flag component)
                 (getf (component-effective-options component) :flags)))
      (and (conditional-feature component)
           (find (conditional-feature component) *features*))))

(define-asdf/interface-class when-condition (conditional-component)
  ())

(define-asdf/interface-class unless-condition (conditional-component)
  ())

(defmethod test-condition ((c unless-condition))
  (not (call-next-method)))

(define-component-name-resolver :when (parent)
  (declare (ignore parent))
  (find-class 'when-condition))

(define-component-name-resolver :unless (parent)
  (declare (ignore parent))
  (find-class 'when-condition))
