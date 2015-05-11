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
   #:c-compiler
   #:executable
   #:invoke
   #:*default-compiler*
   #:c-preprocess
   #:c-assemble
   #:c-link
   #:c-compile
   #:clang
   #:gcc
   #:ensure-compiler)
  ;; components.lisp
  (:export
   #:flag-component
   #:component-direct-flags
   #:component-effective-flags
   
   #:multi-type-source-file
   #:file-types
   
   #:c-file
   #:c-header
   #:c++-file
   #:c++-header
   #:program
   #:make)
  ;; operations.lisp
  (:export
   #:*default-flags*
   #:c-compiler-op
   #:operation-direct-flags
   #:operation-effective-flags
   #:operation-compiler-function
   #:operation-compiler
   #:execute
   
   #:compute-flags-op
   #:preprocess-op
   #:assemble-op
   #:link-op)
  ;; systems.lisp
  (:export
   #:c-system
   #:default-header-class
   #:c-system-compiler
   #:c-system-output
   #:c-system-shared-library
   
   #:c++-system
   
   #:*standard-asdf-class-for-type*
   #:*default-header-class*
   
   #:preprocess-system
   #:assemble-system
   #:link-system)
  ;; toolkit.lisp
  (:export
   #:externalize
   #:shellify
   #:with-cleaned-files
   #:merge-flags
   #:component-path
   #:minimal-shell-namestring
   #:version<))
