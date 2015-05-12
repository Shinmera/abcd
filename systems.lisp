#|
 This file is a part of ABCD
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.abcd)

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
(defvar *default-header-class* 'c-header)

(define-component-name-resolver :header (parent)
  (uiop/utility:coerce-class
   (or (loop for p = parent then (asdf:component-parent p)
             while p thereis (and (typep p 'c-system) (default-header-class p)))
       *default-header-class*)
   :package :asdf/interface :super 'asdf:component :error nil))

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

(defmacro define-operate-delegator (from-op to-op)
  `(defmethod asdf:operate ((op ,from-op) (system c-system) &rest args)
     (apply #'call-next-method ',to-op system args)))

(define-operate-delegator asdf:compile-op link-op)
(define-operate-delegator asdf:load-op link-op)
(define-operate-delegator asdf:program-op link-op)

;; This method is run after the usual around for flag-components. We
;; don't need to save the flags, only overwrite them with the correct
;; merging order for systems. Operation flags should override system
;; flags while other component flags override all.
(defmethod asdf:perform :before ((op c-compiler-op) (system c-system))
  (setf (operation-effective-flags op)
        (merge-flags (operation-direct-flags op) (component-effective-flags system))))

(defmethod asdf:operate :around ((op c-compiler-op) (system c-system) &key)
  (let ((origdir (uiop:getcwd)))
    (unless (operation-compiler op)
      (setf (operation-compiler op)
            (ensure-compiler (c-system-compiler system))))
    (uiop:chdir (component-output-pathname system))
    (unwind-protect
         (call-next-method)
      (uiop:chdir origdir))))

(defmacro define-operation-wrapper (name operation-class)
  `(defun ,name (system &rest args &key flags compiler force force-not verbose version &allow-other-keys)
     (declare (ignore force force-not verbose version))
     (apply #'asdf:operate
            (make-instance ',operation-class
                           :flags flags
                           :compiler (when compiler (ensure-compiler compiler)))
            system args)
     T))

(define-operation-wrapper preprocess-system preprocess-op)
(define-operation-wrapper assemble-system assemble-op)
(define-operation-wrapper link-system link-op)
