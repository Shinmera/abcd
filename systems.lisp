#|
 This file is a part of ABCD
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.abcd)

(define-asdf/interface-class c-system (asdf:system option-component)
  ((default-header-class :initarg :default-header-class :initform 'c-header :accessor default-header-class)
   (compiler :initarg :compiler :initform T :accessor c-system-compiler)
   (shared-library :initarg :shared-library :initform NIL :accessor c-system-shared-library)))

;; We have to do it like this due to kludges in field defaulting. Why oh why, ASDF.
(defmethod asdf/component:component-build-operation ((system c-system))
  (or (call-next-method)
      'link-op))

(defmethod asdf/component:module-default-component-class ((system c-system))
  (or (call-next-method)
      'c-file))

(defun c-system-init (system)
  (unless (asdf/system:component-build-pathname system)
    (setf (asdf/system:component-build-pathname system)
          (asdf:component-name system)))
  (setf (asdf/system:component-build-pathname system)
        (merge-pathnames (asdf/system:component-build-pathname system)
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
  (values (list (asdf/system:component-build-pathname system)) T))

(defmethod asdf:perform ((op link-op) (system c-system))
  (execute op
           (asdf:output-files 'assemble-op system)
           (list (asdf/system:component-build-pathname system))))

(defmacro define-operate-delegator (from-op to-op)
  `(defmethod asdf:operate ((op ,from-op) (system c-system) &rest args)
     (apply #'call-next-method ',to-op system args)))

(define-operate-delegator asdf:compile-op link-op)
(define-operate-delegator asdf:load-op link-op)
(define-operate-delegator asdf:program-op link-op)

;; This method is run after the usual around for option-components. We
;; don't need to save the options, only overwrite them with the correct
;; merging order for systems. Operation options should override system
;; options while other component options override all.
(defmethod asdf:perform :before ((op c-compiler-op) (system c-system))
  (setf (operation-effective-options op)
        (merge-options (operation-direct-options op) (component-effective-options system))))

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
  `(define-asdf/interface-function ,name (system &rest args &key options compiler force force-not verbose version &allow-other-keys)
     (declare (ignore force force-not verbose version))
     (apply #'asdf:operate
            (make-instance ',operation-class
                           :options options
                           :compiler (when compiler (ensure-compiler compiler)))
            system args)
     T))

(define-operation-wrapper preprocess-system preprocess-op)
(define-operation-wrapper assemble-system assemble-op)
(define-operation-wrapper link-system link-op)
