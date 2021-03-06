#|
 This file is a part of ABCD
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.abcd)

(defgeneric c-system-includable-pathnames (system)
  (:method ((system c-system))
    (let ((includes ()))
      (dolist (header (find-components 'c-header system :test #'typep))
        (asdf:operate 'compute-options-op header)
        (when (getf (component-effective-options header) :includable)
          (pushnew (uiop:pathname-directory-pathname
                    (asdf:component-pathname header))
                   includes :test #'equal)))
      includes)))

(defgeneric c-system-library-options (system)
  (:method ((system c-system))
    (assert (or (c-system-shared-library system)
                (c-system-static-library system)))
    `(:include-dirs ,(c-system-includable-pathnames system)
      :library-dirs (,(uiop:pathname-directory-pathname
                       (asdf/system:component-build-pathname system)))
      :libraries (,(pathname-name (asdf/system:component-build-pathname system))))))

(define-dependency-def-parser :shared-library (definition)
  definition)

(define-dependency-resolver :shared-library (system dependant &key)
  (let ((dependant (asdf:find-system dependant T)))
    (nmerge-options (component-direct-options system)
                    (c-system-library-options dependant))
    dependant))

(define-dependency-def-parser :static-library (definition)
  definition)

(define-dependency-resolver :static-library (system dependant &key)
  (let ((dependant (asdf:find-system dependant T)))
    (nmerge-options (component-direct-options system)
                    (c-system-library-options dependant))
    dependant))
