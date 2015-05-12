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
  `(format NIL ,(format NIL "~{~~@[~a ~~]~}" (mapcar #'first options))
           ,@(apply #'append (mapcar #'rest options))))

(defmacro with-cleaned-files ((files form) &body body)
  `(let ((,files ,form))
     (unwind-protect
          (progn ,@body)
       (mapcar #'uiop:delete-file-if-exists ,files))))

(defmacro define-asdf/interface-class (name direct-superclasses direct-slots &rest options)
  `(progn
     (import ',name :asdf/interface)
     (export ',name :asdf/interface)
     (export ',name :asdf)
     (defclass ,name ,direct-superclasses
       ,direct-slots
       ,@options)))

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

(defun assembled-file (pathname)
  (make-pathname :type "o" :defaults pathname))

(defvar *nothing* (make-symbol "NOTHING"))
(defun merge-flags (flags &optional defaults)
  ;; Merge inexistent
  (loop for (flag value) on defaults by #'cddr
        do (when (eql (getf flags flag *nothing*) *nothing*)
             (push value flags)
             (push flag flags)))
  ;; Handle multiples
  ;; (loop for (flag . value) on flags by #'cddr
  ;;       when (consp value)
  ;;       do ())
  flags)

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

(defun component-output-pathname (component)
  (funcall asdf::*output-translation-function*
           (asdf:component-pathname component)))

(defun minimal-shell-namestring (pathname)
  (uiop:native-namestring
   (uiop:enough-pathname
    pathname (uiop:getcwd))))

(defun decode-version (version)
  (let ((parts ())
        (output (make-string-output-stream)))
    (flet ((pushpart ()
             (push (parse-integer (get-output-stream-string output)) parts)
             (setf output (make-string-output-stream))))
      (loop for char across version
            do (case char
                 (#\. (pushpart))
                 (T (write-char char output)))
            finally (pushpart)))
    (nreverse parts)))

(defun version< (lower higher)
  (loop for l in (decode-version lower)
        for h in (decode-version higher)
        do (when (> l h)
             (return NIL))
           (when (< l h)
             (return T))))
