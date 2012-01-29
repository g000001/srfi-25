;;;; srfi-25.asd

(cl:in-package :asdf)

(defsystem :srfi-25
  :serial t
  :depends-on (:fiveam
               :srfi-5
               :srfi-9
               :srfi-23)
  :components ((:file "package")
               (:file "util")
               (:file "srfi-25")
               (:file "test")))

(defmethod perform ((o test-op) (c (eql (find-system :srfi-25))))
  (load-system :srfi-25)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :srfi-25.internal :srfi-25))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))
