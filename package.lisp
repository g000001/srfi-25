;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :srfi-25
  (:use)
  (:export :array? :make-array :shape
           :array :array-rank :array-start
           :array-end :array-ref :array-set!
           :share-array))

(defpackage :srfi-25.internal
  (:use :srfi-25 :cl :fiveam)
  (:shadowing-import-from :srfi-5 :let)
  (:shadowing-import-from :srfi-23 :error)
  (:shadowing-import-from :srfi-25
                          :array
                          :make-array
                          :array-rank)
  (:shadow :lambda :loop :do))

;;; eof
