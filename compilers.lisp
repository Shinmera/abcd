#|
 This file is a part of ABCD
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.abcd)

(defvar *default-compiler* (make-instance 'c-compiler))

(defclass c-compiler ()
  ((executable :initarg :executable :reader executable))
  (:default-initargs :executable "cc"))

(defun ensure-compiler (thing)
  (etypecase thing
    ((eql T) *default-compiler*)
    (c-compiler thing)
    ((or string symbol)
     (ensure-compiler (list thing)))
    (list
     (apply #'make-instance
            (uiop:coerce-class (first thing) :package :org.shirakumo.abcd :super 'c-compiler)
            (rest thing)))))

(defgeneric invoke (c-compiler args &key output error-output &allow-other-keys)
  (:method (c-compiler args &rest kargs &key (output T) (error-output T))
    (let ((command (with-clear-environment
                       (etypecase args
                         (list (list* (executable c-compiler) (mapcar #'externalize args)))
                         (string (format NIL "~a ~a" (executable c-compiler) args))))))
      #+:verbose (v:trace :abcd.compiler "Invoking ~a" command)
      (apply #'uiop:run-program command :output output :error-output error-output kargs))))

(defmacro  define-compiler-method (name (compiler) &body flags)
  (let ((flagargs (remove-duplicates
                   (loop for flag in flags
                         for arg = (loop for arg in flag when (and (symbolp arg) (not (eql arg T))) return arg)
                         when arg collect arg)))
        (c-compiler (gensym "C-COMPILER"))
        (from (gensym "FROM"))
        (to (gensym "TO")))
    `(defmethod ,name ((,c-compiler ,compiler) ,from ,to &key ,@flagargs)
       (invoke ,c-compiler
               (shellify
                 ,@flags
                 ("-o ~a" ,to)
                 ("~*~:[~:*~:*~a~;~:*~:*~{~a~^ ~}~]~*" ,from (listp ,from)))))))

(defmacro define-standard-compiler-method (name (compiler) &body extra-flags)
  `(define-compiler-method ,name (,compiler)
     ,@extra-flags
     ("-W~(~a~)" warnings)
     ("-O~(~a~)" optimize)
     ("-g~*" debug)
     ("-x~a" language)
     ("-std=~a" standard)
     ("~{-f~(~a~)~^ ~}" flags)
     ("~{-I~a~^ ~}" includes)
     ("~{-L~a~^ ~}" libraries)))

(defgeneric c-preprocess (c-compiler from to &key &allow-other-keys)
  (:method (compiler from to &rest args)
    (apply #'call-next-method (ensure-compiler compiler) from to args)))

(define-standard-compiler-method c-preprocess (c-compiler)
  ("-E~*" T)
  ("-Wp~{,~a~}" preprocessor))

(defgeneric c-assemble (c-compiler from to &key &allow-other-keys)
  (:method (compiler from to &rest args)
    (apply #'call-next-method (ensure-compiler compiler) from to args)))

(define-standard-compiler-method c-assemble (c-compiler)
  ("-c~*" T)
  ("-Wa~{,~a~}" assembler))

(defgeneric c-link (c-compiler from to &key &allow-other-keys)
  (:method (compiler from to &rest args)
    (apply #'call-next-method (ensure-compiler compiler) from to args)))

(define-standard-compiler-method c-link (c-compiler)
  ("-shared~*" shared)
  ("-Wl~{,~a~}" linker))

(defgeneric c-compile (c-compiler from to &key &allow-other-keys)
  (:method (compiler from to &rest args)
    (apply #'call-next-method (ensure-compiler compiler) from to args))
  (:method (c-compiler from to &rest args &key options
                                               (preprocess T)
                                               (assemble T)
                                               (link T))
    (flet ((call-with (type function from to)
             (apply function
                    from to
                    (find-symbol (string type) :org.shirakumo.abcd) (getf options type)
                    args)))
      (cond ((and preprocess assemble link)
             (with-cleaned-files (processed (mapcar #'processed-file from))
               (with-cleaned-files (shared (mapcar #'assembled-file from))
                 (loop for input in from
                       for output in processed
                       do (call-with :preprocessor #'c-preprocess input output))
                 (loop for input in processed
                       for output in shared
                       do (call-with :assembler #'c-assemble input output))
                 (call-with :linker #'c-link shared to))))
            ((and preprocess assemble (not link))
             (with-cleaned-files (processed (mapcar #'processed-file from))
               (loop for input in from
                     for output in processed
                     do (call-with :preprocessor #'c-preprocess input output))
               (call-with :assembler #'c-assemble processed to)))
            ((and preprocess (not assemble) (not link))
             (call-with :preprocessor #'c-preprocess from to))
            ((and (not preprocess) assemble link)
             (with-cleaned-files (shared (mapcar #'assembled-file from))
               (loop for input in from
                     for output in shared
                     do (call-with :assembler #'c-assemble input output))
               (call-with :linker #'c-link shared to)))
            ((and (not preprocess) (not assemble) link)
             (call-with :linker #'c-link from to))))
    to))

(defclass clang (c-compiler)
  ()
  (:default-initargs :executable "clang"))

(defclass gcc (c-compiler)
  ()
  (:default-initargs :executable "gcc"))
