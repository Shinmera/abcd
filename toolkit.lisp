#|
 This file is a part of ABCD
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.abcd)

(defun externalize (thing)
  (typecase thing
    (pathname (uiop:native-namestring thing))
    (string thing)
    (T (princ-to-string thing))))

(defmacro shellify (&body options)
  `(format NIL ,(format NIL "~{~~@[~a~~]~^ ~}" (mapcar #'first options))
           ,@(apply #'append (mapcar #'rest options))))

(defmacro with-cleaned-files ((files form) &body body)
  `(let ((,files ,form))
     (unwind-protect
          (progn ,@body)
       (mapcar #'uiop:delete-file-if-exists ,files))))

(defmacro define-asdf/interface-class (name direct-superclasses direct-slots &rest options)
  (unintern name :org.shirakumo.abcd)
  (let ((realname (intern (symbol-name name) :asdf/interface)))
    (import realname :org.shirakumo.abcd)
    `(progn
       (export ',realname :asdf/interface)
       (export ',realname :asdf)
       (import ',realname :org.shirakumo.abcd)
       (defclass ,realname ,direct-superclasses
         ,direct-slots
         ,@options))))

(defun processed-file (pathname)
  (flet ((type= (type) (string-equal (pathname-type pathname) type)))
    (make-pathname
     :type (cond ((or (type= "cc")
                      (type= "cp")
                      (type= "cxx")
                      (type= "cpp")
                      (type= "c++"))
                  "ii")
                 ((or (type= "c"))
                  "i")
                 (T
                  (warn "Unknown file type to preprocess: ~a"
                        (pathname-type pathname))
                  "i"))
     :defaults pathname)))

(defun merge-flags (flags &optional defaults)
  ;; Merge inexistent
  (loop for (flag . value) on defaults by #'cddr
        unless (getf flags flag)
        do (push value flags)
           (push flag flags))
  ;; Handle multiples
  ;; (loop for (flag . value) on flags by #'cddr
  ;;       when (consp value)
  ;;       do ())
  )

(defun assembled-file (pathname)
  (make-pathname :type "o" :defaults pathname))

(defun component-path (component)
  (let ((comps ()))
    (flet ((push-component (c)
             (push
              (etypecase c
                (asdf:source-file (format NIL "~a.~a" (asdf:component-name c) (asdf:file-type c)))
                (asdf:component (asdf:component-name c)))
              comps)))
      (loop for parent = component
            then (asdf:component-parent parent)
            while parent
            do (push-component parent)))
    (format NIL "~{~a~^/~}" comps)))
