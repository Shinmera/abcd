#|
 This file is a part of ABCD
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.abcd)

(define-asdf/interface-class c-system (asdf:system option-component)
  ((default-header-class :initarg :default-header-class :initform 'c-header :accessor default-header-class)
   (compiler :initarg :compiler :initform T :accessor c-system-compiler)
   (shared-library :initarg :shared-library :initform NIL :accessor c-system-shared-library)
   (static-library :initarg :static-library :initform NIL :accessor c-system-static-library)))

;; We have to do it like this due to kludges in field defaulting. Why oh why, ASDF.
(defmethod asdf/component:component-build-operation ((system c-system))
  (or (call-next-method)
      'link-op))

(defmethod asdf/component:module-default-component-class ((system c-system))
  (or (call-next-method)
      'c-file))

(defun c-system-init (system)
  ;; Default output pathname
  (unless (asdf/system:component-build-pathname system)
    (setf (asdf/system:component-build-pathname system)
          (make-pathname :name (asdf:component-name system))))
  ;; Adapt flags for dynamic libs
  (when (c-system-shared-library system)
    (nmerge-options (component-direct-options system) `(:flags ("PIC")
                                                        :shared T)))
  ;; Make absolute to source directory if possible
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

(defmacro define-downward-appending (name op component)
  `(defmethod ,name ((op ,op) (component ,component))
     (loop for child in (asdf:component-children component)
           append (,name op child))))

(define-downward-appending asdf:input-files preprocess-op asdf:parent-component)
(define-downward-appending asdf:input-files assemble-op asdf:parent-component)
(define-downward-appending asdf:input-files link-op asdf:parent-component)
(define-downward-appending asdf:output-files preprocess-op asdf:parent-component)
(define-downward-appending asdf:output-files assemble-op asdf:parent-component)

(defmethod asdf:output-files ((op link-op) (system c-system))
  (values (list (if (or (getf (component-direct-options system) :shared)
                        (getf (component-effective-options system) :shared))
                    (sharedobject-file (asdf/system:component-build-pathname system))
                    (asdf/system:component-build-pathname system))) T))

(defmethod asdf:output-files ((op archive-op) (system c-system))
  (values (list (archive-file (asdf/system:component-build-pathname system))) T))

(defmethod asdf:perform ((op link-op) (system c-system))
  (execute op
           (asdf:output-files 'assemble-op system)
           (list (asdf/system:component-build-pathname system))))

(defmethod asdf:perform ((op archive-op) (system c-system))
  (execute op
           (asdf:output-files 'assemble-op system)
           (list (asdf/system:component-build-pathname system))))

(defmethod asdf:perform ((op compute-options-op) (system c-system))
  (setf (component-effective-options system)
        (merge-options (component-effective-options system)
                       (component-direct-options system))))

;; This method is run after the usual around for option-components. We
;; don't need to save the options, only overwrite them with the correct
;; merging order for systems. Operation options should override system
;; options while other component options override all.
(defmethod asdf:perform :before ((op c-compiler-op) (system c-system))
  (setf (operation-effective-options op)
        (merge-options (operation-direct-options op) (component-effective-options system))))

(defmethod asdf:operate :around ((op c-compiler-op) (system c-system) &key)
  (with-preserved-cwd ((component-output-pathname system))
    (unless (operation-compiler op)
      (setf (operation-compiler op)
            (ensure-compiler (c-system-compiler system))))
    (call-next-method)))

(defmethod asdf:operate :around ((op archive-op) (system c-system) &key)
  (with-preserved-cwd ((component-output-pathname system))
    (call-next-method)))

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
(define-operation-wrapper archive-system archive-op)

(defmethod asdf:operate ((op asdf:load-op) (system c-system) &rest args)
  (apply #'call-next-method 'asdf:compile-op system args))

;; Spoof COMPILE-OP planning to create a plan for LINK-OP and ARCHIVE-OP as needed.
(defmethod asdf/plan:traverse-action :around ((plan asdf/plan:plan) (op asdf:compile-op) (system c-system) needed-in-image-p)
  (when (or (c-system-shared-library system)
            (not (c-system-static-library system)))
    (funcall #'call-next-method plan (asdf:make-operation 'link-op) system needed-in-image-p))
  (when (c-system-static-library system)
    (funcall #'call-next-method plan (asdf:make-operation 'archive-op) system needed-in-image-p))
  plan)
