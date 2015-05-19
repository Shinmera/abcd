#|
 This file is a part of ABCD
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.abcd)

(defun ensure-list (thing)
  (if (listp thing)
      thing
      (list thing)))

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

(defun make-asdf-visible (symbol)
  (import symbol :asdf/interface)
  (export symbol :asdf/interface)
  (export symbol :asdf))

(defmacro define-asdf/interface-class (name direct-superclasses direct-slots &rest options)
  `(progn
     (make-asdf-visible ',name)
     (defclass ,name ,direct-superclasses
       ,direct-slots
       ,@options)))

(defmacro define-asdf/interface-function (name args &body forms)
  `(progn
     (make-asdf-visible ',name)
     (defun ,name ,args
       ,@forms)))

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

(defun archive-file (pathname)
  (make-pathname :type "a"
                 :defaults pathname))

(defun sharedobject-file (pathname)
  (make-pathname :type #+unix "so"
                       #+windows "dll"
                 :defaults pathname))

(defvar *nothing* (make-symbol "NOTHING"))
(defun merge-options (options &optional defaults)
  ;; Merge
  (loop for (option value) on defaults by #'cddr
        for exval = (getf options option *nothing*)
        do (unless (string= "-" option :end2 1)
             (cond ((eql exval *nothing*)
                    (push value options)
                    (push option options))
                   ((and (listp value) (listp exval))
                    (dolist (item (reverse value))
                      (pushnew item (getf options option) :test #'equal))))))
  ;; Process removals
  (loop for (option value) on options by #'cddr
        do (when (string= "-" option :end2 1)
             (let ((realoption (find-symbol (subseq (string option) 1) "KEYWORD")))
               (cond ((eql value T)
                      (remf options realoption))
                     ((listp value)
                      (setf (getf options realoption)
                            (remove-if (lambda (a) (find a value :test #'equal))
                                       (getf options realoption))))))
             (remf options option)))
  options)

(defmacro nmerge-options (options new-flags)
  `(setf ,options
         (merge-options ,new-flags ,options)))

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

(defun with-clear-environment (command)
  #+unix
  (etypecase command
    (cons (list* "env" "-i" command))
    (string (format NIL "env -i ~a" command)))
  #-unix
  command)

(defun find-components (item component &key (key #'identity) (test #'eql))
  (let ((found ()))
    (labels ((traverse (component)
               (when (typep component 'asdf:parent-component)
                 (dolist (child (asdf:component-children component))
                   (when (funcall test (funcall key child) item)
                     (push child found))
                   (traverse child)))))
      (traverse component))
    found))

(defmacro define-function-map-wrappers (base-name &optional (map-test ''equal))
  (let ((map-name (intern (format NIL "*~a-MAP*" base-name)))
        (remove-name (intern (format NIL "REMOVE-~a" base-name)))
        (function (gensym "FUNCTION"))
        (name (gensym "NAME")))
    `(progn
       (defvar ,map-name (make-hash-table :test ,map-test))
       (defun ,base-name (,name)
         (gethash (string ,name) ,map-name))
       (defun (setf ,base-name) (,function ,name)
         (setf (gethash (string ,name) ,map-name)
               ,function))
       (defun ,remove-name (,name)
         (remhash (string ,name) ,map-name)))))
