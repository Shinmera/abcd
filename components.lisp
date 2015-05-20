#|
 This file is a part of ABCD
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.abcd)

(define-asdf/interface-class option-component (asdf:component)
  ((direct-options :initform () :initarg :options :accessor component-direct-options)
   (effective-options :initform () :accessor component-effective-options)))

(defun find-options-parent (component)
  (loop for parent = (asdf:component-parent component)
        then (asdf:component-parent parent)
        while (typep parent 'asdf:child-component)
        when (typep parent 'option-component)
        do (return parent)))

(defmethod asdf:perform ((op compute-options-op) (component option-component))
  (let ((parent (find-options-parent component)))
    (setf (component-effective-options component)
          (if parent
              (merge-options (component-direct-options component)
                             (component-effective-options parent))
              (component-direct-options component)))))

(defmethod asdf:perform :around ((op c-compiler-op) (component option-component))
  (let ((current-options (operation-effective-options op)))
    (unwind-protect
         (progn
           (setf (operation-effective-options op)
                 (merge-options (component-effective-options component) (operation-effective-options op)))
           (call-next-method))
      (setf (operation-effective-options op) current-options))))

(define-asdf/interface-class multi-type-source-file (asdf:source-file)
  ((types :initform () :initarg :types :accessor file-types)
   (filename :initarg :filename :accessor file-name)))

(defun initialize-multi-type-source-file (file)
  (let ((filename (asdf:component-name file)))
    (let ((lastdot (position #\. (asdf:component-name file) :from-end T)))
      (when lastdot
        (let ((extension (find (subseq (asdf:component-name file) (1+ lastdot))
                               (file-types file) :test #'string=)))
          (when extension
            (setf (asdf:file-type file) extension)
            (setf filename (subseq (asdf:component-name file) 0 lastdot))))))
    (setf (file-name file) filename))
  (setf (asdf:component-name file)
        (format NIL "~a.~a"
                (file-name file)
                (asdf:file-type file))))

(defmethod asdf:component-pathname ((file multi-type-source-file))
  (make-pathname :name (file-name file) :type (asdf:file-type file)
                 :defaults (uiop:pathname-directory-pathname (asdf/component:component-parent-pathname file))))

(defmethod initialize-instance :after ((file multi-type-source-file) &key)
  (initialize-multi-type-source-file file))

(defmethod reinitialize-instance :after ((file multi-type-source-file) &key)
  (initialize-multi-type-source-file file))

(define-asdf/interface-class c-file (multi-type-source-file option-component)
  ((type :initform "c"))
  (:default-initargs :types '("c")))

(defmethod print-object ((file c-file) stream)
  (print-unreadable-object (file stream :type T)
    (format stream "~a" (component-path file))))

(defmethod asdf:action-description ((op preprocess-op) (file c-file))
  (format nil "~@<calling the preprocessor on ~3i~_~A~@:>" file))

(defmethod asdf:input-files ((op preprocess-op) (file c-file))
  (list (asdf:component-pathname file)))

(defmethod asdf:output-files ((op preprocess-op) (file c-file))
  (list (make-pathname :name (file-name file) :type "i")))

(defmethod asdf:action-description ((op assemble-op) (file c-file))
  (format nil "~@<calling the assembler on ~3i~_~A~@:>" file))

(defmethod asdf:input-files ((op assemble-op) (file c-file))
  (asdf:output-files 'preprocess-op file))

(defmethod asdf:output-files ((op assemble-op) (file c-file))
  (list (make-pathname :name (file-name file) :type "o")))

(defmethod asdf:input-files ((op link-op) (file c-file))
  (asdf:output-files 'assemble-op file))

(defmethod asdf:perform ((op preprocess-op) (file c-file))
  (execute op (asdf:input-files op file) (asdf:output-files op file)))

(defmethod asdf:perform ((op assemble-op) (file c-file))
  (execute op (asdf:input-files op file) (asdf:output-files op file)))

(defmethod asdf:perform ((op asdf:compile-op) (file c-file))
  (asdf:perform 'assemble-op file))

(defmethod asdf:perform ((op asdf:load-op) (file c-file))
  NIL)

(define-asdf/interface-class c-header (multi-type-source-file option-component)
  ((type :initform "h"))
  (:default-initargs :types '("h")))

(defmethod asdf:perform ((op asdf:compile-op) (file c-header))
  NIL)

(defmethod asdf:perform ((op asdf:load-op) (file c-header))
  NIL)

(define-asdf/interface-class c++-file (c-file)
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
