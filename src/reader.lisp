(defpackage lisp-database:reader
  (:nicknames "ldb:reader")
  (:use :cl :ldb:table))
(in-package :lisp-database:reader)

(defparameter *ldb-stream* *standard-input*
  "This parameter will hold the stream to the lisp-database table to be read.")

(defparameter *reader-state* nil
  "Hold the state of the reader while reading the content of the stream.")

(defparameter *data-table* nil
  "The data table resulting from reading the stream.")

(defparameter *field-definition* nil
  "Table's field definition.")

(defun red-ldb-field (ldb-stream eof-separator)
  "Read the content of a lisp expression considering it is a ldb field definition."
  (progn
    ))




(defun read-ldb-table (ldb-stream eof-separator)
  "Read the content of a lisp database and load it into memory."
  (let
      ((parse-result (read ldb-stream nil eof-separator)))
    (cond ((typep parse-result 'symbol)
           (cond ((eq parse-result 'TABLE)
                  (setf *reader-state* :TABLE-DEFINITION))
                 ((eq parse-result 'FIELD)
                  (setf *reader-state* :FIELD-DEFINITION))
                 ((eq parse-result 'VERSION)
                  (setf *reader-state* :VERSION-DEFINITION))
                 ((eq *reader-state* :TABLE-DEFINITION)
                  (setf *data-table* (make-instance ldb:table:table :name parse-result))
                  (setf *reader-state* :TABLE-NAME))
                 ((eq *reader-state* :FIELD-DEFINITION)
                  (setf *field-definition* (ldb:table:make-field :name parse-result))
                  (setf *reader-state* :FIELD-NAME))
                 (t
                  (error "Unknown Keyword: ~S" parse-result))))
          ((typep parse-result 'string)
           (cond ((eq *reader-state* :TABLE-NAME)
                  (setf (description *data-table*) parse-result)
                  (setf *reader-state* :TABLE-DESCRIPTION))
                 ((eq *reader-state* :FIELD-NAME)
                  (setf (description *field-definition*) parse-result)
                  (setf *reader-state* :FIELD-DESCRIPTION))))
          ((typep parse-result 'integer)
           (t))
          ((typep parse-result 'float)
           (t))
          ((typep parse-result 'cons)
           ((dolist (elt parse-result)
               (read-ldb-table ldb-stream eof-separator)))
          (t
           (error "Wrong type argument: ~S - Type: ~S" parse-result (type-of parse-result)))
          )
    )
