;;;; package.lisp

(cl:in-package :cl-user)


(defpackage "https://github.com/g000001/srfi-25"
  (:use)
  (:export array? make-array shape
           array array-rank array-start
           array-end array-ref array-set!
           share-array))


(defpackage "https://github.com/g000001/srfi-25#internals"
  (:use 
   "https://github.com/g000001/srfi-25"
   "https://github.com/g000001/srfi-23"
   "https://github.com/g000001/srfi-5"
   cl
   fiveam)
  (:shadowing-import-from
   "https://github.com/g000001/srfi-5" let)
  (:shadowing-import-from
   "https://github.com/g000001/srfi-23" error)
  (:shadowing-import-from
   "https://github.com/g000001/srfi-25"
   array
   make-array
   array-rank)
  (:shadow lambda loop do))


;;; *EOF*
