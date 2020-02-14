(defpackage lisp-database/tests/main
  (:use :cl
        :lisp-database
        :rove))
(in-package :lisp-database/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :lisp-database)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
