#|
 This file is a part of ABCD
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem abcd
  :name "ABCD"
  :version "0.1.0"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "Artistic"
  :description "Asdf Builds C/++ Directly. Extensions to ASDF to allow building C projects."
  :homepage "https://github.com/Shinmera/abcd"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "defsystem")
               (:file "applications")
               (:file "compilers")
               (:file "operations")
               (:file "components")
               (:file "conditionals")
               (:file "systems")
               (:file "dependencies"))
  :depends-on (:trivial-features))
