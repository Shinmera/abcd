#|
 This file is a part of ABCD
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:abcd
  (:nicknames #:org.shirakumo.abcd)
  (:use #:cl)
  ;; compilers.lisp
  (:export
   #:*default-compiler*
   #:c-compiler
   #:executable
   
   #:ensure-compiler
   #:invoke
   #:c-preprocess
   #:c-assemble
   #:c-link
   #:c-compile
   #:clang
   #:gcc)
  ;; components.lisp
  (:export
   #:option-component
   #:component-direct-options
   #:component-effective-options
   
   #:multi-type-source-file
   #:file-types
   
   #:c-file
   #:c-header
   #:c++-file
   #:c++-header
   #:program)
  ;; conditionals.lisp
  (:export
   #:conditional-component
   #:conditional-test
   #:conditional-option
   #:conditional-flag
   #:conditional-feature
   
   #:test-condition
   #:when-condition
   #:unless-condition
   
   #:ensure-conditional-function)
  ;; defsystem.lisp
  (:export
   #:component-name-resolver
   #:remove-component-name-resolver
   #:define-component-name-resolver)
  ;; operations.lisp
  (:export
   #:*default-options*
   #:c-compiler-op
   #:operation-direct-options
   #:operation-effective-options
   #:operation-compiler-function
   #:operation-compiler
   #:execute
   
   #:compute-options-op
   #:preprocess-op
   #:assemble-op
   #:link-op)
  ;; systems.lisp
  (:export
   #:c-system
   #:default-header-class
   #:c-system-compiler
   #:c-system-shared-library
   
   #:c++-system
   
   #:*default-header-class*
   
   #:preprocess-system
   #:assemble-system
   #:link-system)
  ;; toolkit.lisp
  (:export
   #:externalize
   #:shellify
   #:with-cleaned-files
   #:merge-options
   #:component-path
   #:minimal-shell-namestring
   #:version<
   #:with-clear-environment))
