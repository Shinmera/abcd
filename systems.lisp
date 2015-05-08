#|
 This file is a part of ABCD
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.abcd)

(defvar *default-flags* '(:warnings :all))

(define-asdf/interface-class c-system (asdf:system flag-component)
  ((default-header-class :initarg :default-header-class :initform 'c-header :accessor default-header-class)
   (compiler :initarg :compiler :initform T :accessor c-system-compiler)
   (output :initarg :output :accessor c-system-output)
   (shared-library :initarg :shared-library :initform NIL :accessor c-system-shared-library)))

;; We have to do it like this due to kludges in field defaulting. Why oh why, ASDF.
(defmethod asdf/component:module-default-component-class ((system c-system))
  (or (call-next-method)
      'c-file))

(defun c-system-init (system)
  (unless (slot-boundp system 'output)
    (setf (c-system-output system)
          (asdf:component-name system)))
  (setf (c-system-output system)
        (merge-pathnames (c-system-output system)
                         (asdf:system-source-directory system))))

(defmethod initialize-instance :after ((system c-system) &key)
  (c-system-init system))

(defmethod reinitialize-instance :after ((system c-system) &key)
  (c-system-init system))

(define-asdf/interface-class c++-system (c-system)
  ((default-header-class :initarg :default-header-class :initform 'c++-header :accessor default-header-class)))

(defmethod asdf/component:module-default-component-class ((system c++-system))
  (or (call-next-method)
      'c++-file))

;; Allow using :HEADER as well as :FILE in c-system definitions.
(defvar *standard-asdf-class-for-type* #'asdf::class-for-type)
(defvar *default-header-class* 'c-header)

(defun asdf::class-for-type (parent type)
  (or (and (eq type :header)
           (asdf::coerce-class
            (or (loop for p = parent then (asdf:component-parent p)
                      while p
                      thereis (and (typep p 'c-system) (default-header-class p)))
                *default-header-class*)
            :package :asdf/interface :super 'asdf:component :error nil))
      (funcall *standard-asdf-class-for-type* parent type)))

(defmacro define-downard-appending (name op component)
  `(defmethod ,name ((op ,op) (component ,component))
     (loop for child in (asdf:component-children component)
           append (,name op child))))

(define-downard-appending asdf:input-files preprocess-op asdf:parent-component)
(define-downard-appending asdf:input-files assemble-op asdf:parent-component)
(define-downard-appending asdf:input-files link-op asdf:parent-component)
(define-downard-appending asdf:output-files preprocess-op asdf:parent-component)
(define-downard-appending asdf:output-files assemble-op asdf:parent-component)

(defmethod asdf:output-files ((op link-op) (system c-system))
  (values (list (c-system-output system)) T))

(defmethod asdf:perform ((op link-op) (system c-system))
  (execute op (asdf:output-files 'assemble-op system) (list (c-system-output system))))

(defmethod asdf:operate ((op asdf:compile-op) (system c-system) &rest args &key)
  (apply #'asdf:operate 'link-op system args))

(defmethod asdf:operate ((op asdf:load-op) (system c-system) &rest args &key)
  (apply #'asdf:operate 'link-op system args))

(defmethod asdf:operate ((op asdf:program-op) (system c-system) &rest args &key)
  (apply #'asdf:operate 'link-op system args))
