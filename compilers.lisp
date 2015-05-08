#|
 This file is a part of ABCD
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.abcd)

(defclass c-compiler ()
  ((executable :initarg :executable :reader executable :allocation :class))
  (:default-initargs :executable "cc"))

(defgeneric invoke (c-compiler args &key output error-output &allow-other-keys)
  (:method (c-compiler args &rest kargs &key (output T) (error-output T))
    (apply #'uiop:run-program (etypecase args
                                (list (cons (executable c-compiler) (mapcar #'externalize args)))
                                (string (format NIL "~a ~a" (executable c-compiler) args)))
           :output output :error-output error-output kargs)))

(defvar *default-compiler* (make-instance 'c-compiler))

(defgeneric c-preprocess (c-compiler from to &key warnings
                                                  source
                                                  standard
                                                  options
                                                  arguments
                          &allow-other-keys)
  (:method ((compiler T) from to &rest args &key)
    (apply #'c-preprocess *default-compiler* from to args))
  (:method ((c-compiler c-compiler) from to &key (warnings :all)
                                                 source
                                                 standard
                                                 options
                                                 arguments)
    (invoke c-compiler
            (shellify
             ("-E~*" T)
             ("-W~(~a~)" warnings)
             ("-x~a" source)
             ("-std=~a" standard)
             ("-Wp~{,~a~}" options)
             ("~{-Xpreprocessor ~a~^ ~}" arguments)
             ("-o ~a" to)
             ("~a" from)))))

(defgeneric c-assemble (c-compiler from to &key warnings
                                                optimize
                                                debug
                                                source
                                                standard
                                                options
                                                arguments
                        &allow-other-keys)
  (:method ((compiler T) from to &rest args &key)
    (apply #'c-assemble *default-compiler* from to args))
  (:method ((c-compiler c-compiler) from to &key (warnings :all)
                                                 optimize
                                                 debug
                                                 source
                                                 standard
                                                 options
                                                 arguments)
    (invoke c-compiler
            (shellify
              ("-c~*" T)
              ("-W~(~a~)" warnings)
              ("-O~(~a~)" optimize)
              ("-g~*" debug)
              ("-x~a" source)
              ("-std=~a" standard)
              ("-Wa~{,~a~}" options)
              ("~{-Xassembler ~a~^ ~}" arguments)
              ("-o ~a" to)
              ("~a" from)))))

(defgeneric c-link (c-compiler from to &key debug
                                            source
                                            standard
                                            options
                                            arguments
                                            shared
                    &allow-other-keys)
  (:method ((compiler T) from to &rest args &key)
    (apply #'c-link *default-compiler* from to args))
  (:method ((c-compiler c-compiler) from to &key debug
                                                 source
                                                 standard
                                                 options
                                                 arguments
                                                 shared)
    (invoke c-compiler
            (shellify
              ("-g~*" debug)
              ("-x~a" source)
              ("-std=~a" standard)
              ("-Wl~{,~a~}" options)
              ("~{-Xlinker ~a~^ ~}" arguments)
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
                                               arguments
                                               preprocess
                                               assemble
                                               link
                       &allow-other-keys)
  (:method ((compiler T) from to &rest args &key)
    (apply #'c-compile *default-compiler* from to args))
  (:method (c-compiler from to &rest args &key options
                                               arguments
                                               (preprocess T)
                                               (assemble T)
                                               (link T))
    (flet ((call-with (type function from to)
             (apply function
                    from to
                    :options (getf options type)
                    :arguments (getf arguments type)
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
    (c-compiler thing)
    ((or string symbol)
     (ensure-compiler (list thing)))
    (list
     (apply #'make-instance
            (uiop:coerce-class (first thing) :package :org.shirakumo.abcd :super 'c-compiler)
            (rest thing)))))
