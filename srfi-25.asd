;;;; srfi-25.asd

(cl:in-package :asdf)


(defsystem :srfi-25
  :version "20200310"
  :description "SRFI 25 for CL: Multi-dimensional Array Primitives"
  :long-description "SRFI 25 for CL: Multi-dimensional Array Primitives
https://srfi.schemers.org/srfi-25"
  :author "Jussi Piitulainen"
  :maintainer "CHIBA Masaomi"
  :serial t
  :depends-on (:fiveam
               :srfi-5
               :srfi-9
               :srfi-23)
  :components ((:file "package")
               (:file "util")
               (:file "srfi-25")
               (:file "test")))


(defmethod perform :after ((o load-op) (c (eql (find-system :srfi-25))))
  (let ((name "https://github.com/g000001/srfi-25")
        (nickname :srfi-25))
    (if (and (find-package nickname)
             (not (eq (find-package nickname)
                      (find-package name))))
        (warn "~A: A package with name ~A already exists." name nickname)
        (rename-package name name `(,nickname)))))


(defmethod perform ((o test-op) (c (eql (find-system :srfi-25))))
  (let ((*package*
         (find-package
          "https://github.com/g000001/srfi-25#internals")))
    (eval
     (read-from-string
      "
      (or (let ((result (run 'srfi-25)))
            (explain! result)
            (results-status result))
          (error \"test-op failed\") )"))))


;;; *EOF*
