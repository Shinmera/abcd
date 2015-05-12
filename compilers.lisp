#|
 This file is a part of ABCD
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.abcd)

(defclass c-compiler ()
  ((executable :initarg :executable :reader executable))
  (:default-initargs :executable "cc"))

(defgeneric invoke (c-compiler args &key output error-output &allow-other-keys)
  (:method (c-compiler args &rest kargs &key (output T) (error-output T))
    (let ((command (etypecase args
                     (list (cons (executable c-compiler) (mapcar #'externalize args)))
                     (string (format NIL "~a ~a" (executable c-compiler) args)))))
      #+:verbose (v:trace :abcd.compiler "Invoking ~a" command)
      (apply #'uiop:run-program command :output output :error-output error-output kargs))))

(defvar *default-compiler* (make-instance 'c-compiler))

(defgeneric c-preprocess (c-compiler from to &key warnings
                                                  source
                                                  standard
                                                  options
                          &allow-other-keys)
  (:method ((compiler T) from to &rest args &key)
    (apply #'c-preprocess *default-compiler* from to args))
  (:method ((c-compiler c-compiler) from to &key warnings
                                                 source
                                                 standard
                                                 options)
    (invoke c-compiler
            (shellify
             ("-E~*" T)
             ("-W~(~a~)" warnings)
             ("-x~a" source)
             ("-std=~a" standard)
             ("-Wp~{,~a~}" options)
             ("-o ~a" to)
             ("~a" from)))))

(defgeneric c-assemble (c-compiler from to &key warnings
                                                optimize
                                                debug
                                                source
                                                standard
                                                options
                        &allow-other-keys)
  (:method ((compiler T) from to &rest args &key)
    (apply #'c-assemble *default-compiler* from to args))
  (:method ((c-compiler c-compiler) from to &key warnings
                                                 optimize
                                                 debug
                                                 source
                                                 standard
                                                 options)
    (invoke c-compiler
            (shellify
              ("-c~*" T)
              ("-W~(~a~)" warnings)
              ("-O~(~a~)" optimize)
              ("-g~*" debug)
              ("-x~a" source)
              ("-std=~a" standard)
              ("-Wa~{,~a~}" options)
              ("-o ~a" to)
              ("~a" from)))))

(defgeneric c-link (c-compiler from to &key debug
                                            source
                                            standard
                                            options
                                            shared
                    &allow-other-keys)
  (:method ((compiler T) from to &rest args &key)
    (apply #'c-link *default-compiler* from to args))
  (:method ((c-compiler c-compiler) from to &key debug
                                                 source
                                                 standard
                                                 options
                                                 shared)
    (invoke c-compiler
            (shellify
              ("-g~*" debug)
              ("-x~a" source)
              ("-std=~a" standard)
              ("-Wl~{,~a~}" options)
              ("-shared~*" shared)
              ("-o ~a" to)
              ("~:[~a~;~{~a~^ ~}~]" (listp from) from)))))

(defgeneric c-compile (c-compiler from to &key warnings
                                               optimize
                                               debug
                                               source
                                               standard
                                               shared
                                               options
                                               preprocess
                                               assemble
                                               link
                       &allow-other-keys)
  (:method ((compiler T) from to &rest args &key)
    (apply #'c-compile *default-compiler* from to args))
  (:method (c-compiler from to &rest args &key options
                                               (preprocess T)
                                               (assemble T)
                                               (link T))
    (flet ((call-with (type function from to)
             (apply function
                    from to
                    :options (getf options type)
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
