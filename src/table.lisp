(defpackage lisp-database:table
  (:nicknames "ldb:table")
  (:use :cl))
(in-package :lisp-database:table)

;; (FIELD name description type (key nil) (nil-allowed t) (unique nil) (type-specific-options-list nil)
(defstruct field
  name
  description
  type
  key
  (nil-allowed t)
  unique
  options)

  (defclass table ()
    ((name
      :initarg :table
      :initform (error "Must supply a string representing the loaded table.")
      :accessor table)
     (description
      :initarg description
      :initarg ""
      :accessor description)
     (version)
     (fields
      :initarg :fields)
     (records
      :initargs :records)))

(defgeneric set-version (table new-version)
  (:documentation "Set the version of the table as an integer."))

(defgeneric set-version-triplet (table new-major new-minor new-revision)
  (:documentation "Set the version of the table using 3 integers."))

(defgeneric add-field (table field-definition)
  (:documentation "Add a field definitions to the FIELDS' hashtable."))

(defgeneric delete-field (table field-name)
  (:documentation "Remove a field definitions in the FIELDS' hashtable."))

(defgeneric modify-field (table field-name fields-new-values)
  (:documentation "Modify a field definition's in the FIELDS' hashtable."))

(defgeneric get-field (table field-name)
  (:documentation "Return the field definition for FIELD-NAME."))

(defgeneric get-all-fields (table)
  (:documentation "Return all fields definition for TABLE."))

(defgeneric add-record (table record)
  (:documentation "Add a record to the RECORDS' hashtable."))

(defgeneric delete-record (table record-primary-key)
  (:documentation "Remove a record in the RECORDS' hashtable."))

(defgeneric modify-record (table record-primary-key record-new-values)
  (:documentation "Modify some or all data of a record for RECORD-PRIMARY-KEY."))

(defgeneric get-record (table record-primary-key)
  (:documentation "Return the record corresponding to RECORD-PRIMARY-KEY."))

(defgeneric get-all-records (table)
  (:documentation "Return all records in TABLE."))

(defgeneric search-records (table search-criteria)
  (:documentation "Return all records in TABLE, corresponding to SEARCH-CRITERIA."))

(defgeneric get-primary-key-name (table)
  (:documentation "Return the name of the field defined as primary-key of the table."))

