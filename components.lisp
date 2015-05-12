#|
 This file is a part of ABCD
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.abcd)

(define-asdf/interface-class flag-component (asdf:component)
  ((direct-flags :initform () :initarg :flags :accessor component-direct-flags)
   (effective-flags :initform () :accessor component-effective-flags)))

(defmethod asdf:perform ((op compute-flags-op) (component flag-component))
  (setf (component-effective-flags component)
        (if (and (typep component 'asdf:child-component)
                 (typep (asdf:component-parent component) 'flag-component))
            (merge-flags (component-direct-flags component)
                         (component-effective-flags (asdf:component-parent component)))
            (component-direct-flags component))))

(defmethod asdf:perform :around ((op c-compiler-op) (component flag-component))
  (let ((current-flags (operation-effective-flags op)))
    (unwind-protect
         (progn
           (setf (operation-effective-flags op)
                 (merge-flags (component-effective-flags component) (operation-effective-flags op)))
           (call-next-method))
      (setf (operation-effective-flags op) current-flags))))

(define-asdf/interface-class multi-type-source-file (asdf:source-file)
  ((types :initform () :initarg :types :accessor file-types)))

(defmethod initialize-instance :after ((file multi-type-source-file) &key)
  (let ((lastdot (position #\. (asdf:component-name file) :from-end T)))
    (when lastdot
      (let ((extension (find (subseq (asdf:component-name file) (1+ lastdot))
                             (file-types file) :test #'string=)))
        (when extension
          (setf (asdf:file-type file) extension)
          (setf (asdf:component-name file) (subseq (asdf:component-name file) 0 lastdot)))))))

(define-asdf/interface-class c-file (asdf:source-file flag-component)
  ((type :initform "c")))

(defmethod print-object ((file c-file) stream)
  (print-unreadable-object (file stream :type T)
    (format stream "~a" (component-path file))))

(defmethod asdf:action-description ((op preprocess-op) (file c-file))
  (format nil "~@<calling the preprocessor on ~3i~_~A~@:>" file))

(defmethod asdf:action-description ((op assemble-op) (file c-file))
  (format nil "~@<calling the assembler on ~3i~_~A~@:>" file))

(defmethod asdf:input-files ((op preprocess-op) (file c-file))
  (list (asdf:component-pathname file)))

(defmethod asdf:input-files ((op assemble-op) (file c-file))
  (asdf:output-files 'preprocess-op file))

(defmethod asdf:input-files ((op link-op) (file c-file))
  (asdf:output-files 'assemble-op file))

(defmethod asdf:output-files ((op preprocess-op) (file c-file))
  (list (make-pathname :name (asdf:component-name file) :type "i")))

(defmethod asdf:output-files ((op assemble-op) (file c-file))
  (list (make-pathname :name (asdf:component-name file) :type "o")))

(defmethod asdf:perform ((op preprocess-op) (file c-file))
  (execute op (asdf:input-files op file) (asdf:output-files op file)))

(defmethod asdf:perform ((op assemble-op) (file c-file))
  (execute op (asdf:input-files op file) (asdf:output-files op file)))

(defmethod asdf:perform ((op asdf:compile-op) (file c-file))
  (asdf:perform 'assemble-op file))

(define-asdf/interface-class c-header (asdf:source-file)
  ((type :initform "h")))

(define-asdf/interface-class c++-file (c-file multi-type-source-file)
  ((type :initform "cpp"))
  (:default-initargs :types '("c" "cpp" "cxx" "c++" "C" "cc")))

(defmethod asdf:output-files ((op preprocess-op) (file c++-file))
  (list (make-pathname :name (asdf:component-name file) :type "ii")))

(define-asdf/interface-class c++-header (c-header multi-type-source-file)
  ((type :initform "h"))
  (:default-initargs :types '("h" "hpp" "hxx" "h++" "H" "hh")))

(define-asdf/interface-class program (asdf:component)
  ((command :initarg :command :accessor program-command)
   (arguments :initarg :args :initarg :arguments :accessor program-arguments))
  (:default-initargs
   :command (error "COMMAND required.")
   :arguments ()))

(defmethod asdf:perform ((op asdf:compile-op) (program program))
  (uiop:run-program (cons (program-command program)
                          (program-arguments program))
                    :output T :error-output T))

;; (define-asdf/interface-class make (program)
;;   ()
;;   (:default-initargs
;;    :command "make"))

;; (defmethod asdf:perform :around ((op asdf:compile-op) (make make))
;;   (let ((args (program-arguments make)))
;;     (push (asdf:component-name make) (program-arguments make))
;;     (unwind-protect
;;          (call-next-method)
;;       (setf (program-arguments make) args))))
