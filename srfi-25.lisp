;;;; srfi-25.lisp


(cl:in-package "https://github.com/g000001/srfi-25#internals")

;;; cat as-procedure.scm \
;;;     ix-ctor.scm      \
;;;     op-ctor.scm      \
;;;     array.scm        \
;;; > srfi-25-reference.scm

;;; array as-procedure
;;; 1997 - 2001 Jussi Piitulainen

;;; Records are not standard in Scheme. An array representation consists of
;;; three fields: backing vector, index mapping, and shape vector.

;;; The type predicate is approximate.

(define-function (array$make vec idx shp)
  (cons (lambda (token)
          (case token
            ((vec) vec)
            ((idx) idx)
            ((shp) shp)))
        'array))

(define-function (array$vector array)
  (funcall (car array) 'vec))

(define-function (array$index array)
  (funcall (car array) 'idx))

(define-function (array$shape array)
  (funcall (car array) 'shp))

(define-function (array$array? obj)
  (and (pair? obj)
       (procedure? (car obj))
       (eq? (cdr obj) 'array)))
(define-function (array-ref a . xs)
  (or (array$array? a)
      (error "not an array"))
  (let ((shape (array$shape a)))
    (if (null? xs)
        (array$check-indices "array-ref" xs shape)
        (let ((x (car xs)))
          (if (vector? x)
              (array$check-index-vector "array-ref" x shape)
              (if (integer? x)
                  (array$check-indices "array-ref" xs shape)
                  (if (array$array? x)
                      (array$check-index-actor "array-ref" x shape)
                      (error "not an index object"))))))
    (vector-ref
     (array$vector a)
     (if (null? xs)
         (vector-ref (array$index a) 0)
         (let ((x (car xs)))
           (if (vector? x)
               (array$index/vector
                (quotient (vector-length shape) 2)
                (array$index a)
                x)
               (if (integer? x)
                   (array$vector-index (array$index a) xs)
                   (if (array$array? x)
                       (array$index/array
                        (quotient (vector-length shape) 2)
                        (array$index a)
                        (array$vector x)
                        (array$index x))
                       (error "array-ref: bad index object")))))))))

(define-function (array-set! a x . xs)
  (or (array$array? a)
      (error "array-set!: not an array"))
  (let ((shape (array$shape a)))
    (if (null? xs)
        (array$check-indices "array-set!" '() shape)
        (if (vector? x)
            (array$check-index-vector "array-set!" x shape)
            (if (integer? x)
                (array$check-indices.o "array-set!" (cons x xs) shape)
                (if (array$array? x)
                    (array$check-index-actor "array-set!" x shape)
                    (error "not an index object")))))
    (if (null? xs)
        (vector-set! (array$vector a) (vector-ref (array$index a) 0) x)
        (if (vector? x)
            (vector-set! (array$vector a)
                         (array$index/vector
                          (quotient (vector-length shape) 2)
                          (array$index a)
                          x)
                         (car xs))
            (if (integer? x)
                (let ((v (array$vector a))
                      (i (array$index a))
                      (r (quotient (vector-length shape) 2)))
                  (do ((sum (* (vector-ref i 0) x)
                            (+ sum (* (vector-ref i k) (car ks))))
                       (ks xs (cdr ks))
                       (k 1 (+ k 1)))
                    ((= k r)
                     (vector-set! v (+ sum (vector-ref i k)) (car ks)))))
                (if (array$array? x)
                    (vector-set! (array$vector a)
                                 (array$index/array
                                  (quotient (vector-length shape) 2)
                                  (array$index a)
                                  (array$vector x)
                                  (array$index x))
                                 (car xs))
                    (error (string-append
                            "array-set!: bad index object: "
                            (array$thing->string x)))))))))

(defvar array$opt-args '(ctor (4)))
(define-function (array$optimize f r)
  (case r
    ((0) (let ((n0 (funcall f))) (array$0 n0)))
    ((1) (let ((n0 (funcall f 0))) (array$1 n0 (- (funcall f 1) n0))))
    ((2)
     (let ((n0 (funcall f 0 0)))
       (array$2 n0 (- (funcall f 1 0) n0) (- (funcall f 0 1) n0)) ))
    ((3)
     (let ((n0 (funcall f 0 0 0)))
       (array$3
        n0
        (- (funcall f 1 0 0) n0)
        (- (funcall f 0 1 0) n0)
        (- (funcall f 0 0 1) n0) )))
    (otherwise
     (let ((v
            (do ((k 0 (+ k 1)) (v '() (cons 0 v)))
                ((= k r) v) )))
       (let ((n0 (apply f v)))
         (apply
          #'array$n
          n0
          (array$coefficients f n0 v v) ))))))
(define-function (array$optimize-empty r)
  (let ((x (make-vector (+ r 1) 0)))
    (vector-set! x r -1)
    x ))
(define-function (array$coefficients f n0 vs vp)
  (case vp
    ((()) '())
    (otherwise
     (setf (car vp) 1)
     (let ((n (- (apply f vs) n0)))
       (setf (car vp) 0)
       (cons n (array$coefficients f n0 vs (cdr vp))) ))))
(define-function (array$vector-index x ks)
  (do ((sum 0 (+ sum (* (vector-ref x k) (car ks))))
       (ks ks (cdr ks))
       (k 0 (+ k 1)) )
      ((null? ks) (+ sum (vector-ref x k))) ))
(define-function (array$shape-index) '#(2 1 0))
(define-function (array$empty-shape-index) '#(0 0 -1))
(define-function (array$shape-vector-index x r k)
  (+
   (* (vector-ref x 0) r)
   (* (vector-ref x 1) k)
   (vector-ref x 2) ))
(define-function (array$actor-index x k)
  (+ (* (vector-ref x 0) k) (vector-ref x 1)) )
(define-function (array$0 n0) (vector n0))
(define-function (array$1 n0 n1) (vector n1 n0))
(define-function (array$2 n0 n1 n2) (vector n1 n2 n0))
(define-function (array$3 n0 n1 n2 n3) (vector n1 n2 n3 n0))
(define-function (array$n n0 n1 n2 n3 n4 . ns)
  (apply #'vector n1 n2 n3 n4 (append ns (list n0))) )
(define-function (array$maker r)
  (case r
    ((0) #'array$0)
    ((1) #'array$1)
    ((2) #'array$2)
    ((3) #'array$3)
    (otherwise #'array$n) ))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-function array$indexer/vector
    (let ((em
         (vector
          (lambda (x i) (declare (ignore i)) (+ (vector-ref x 0)))
          (lambda (x i)
            (+
             (* (vector-ref x 0) (vector-ref i 0))
             (vector-ref x 1) ))
          (lambda (x i)
            (+
             (* (vector-ref x 0) (vector-ref i 0))
             (* (vector-ref x 1) (vector-ref i 1))
             (vector-ref x 2) ))
          (lambda (x i)
            (+
             (* (vector-ref x 0) (vector-ref i 0))
             (* (vector-ref x 1) (vector-ref i 1))
             (* (vector-ref x 2) (vector-ref i 2))
             (vector-ref x 3) ))
          (lambda (x i)
            (+
             (* (vector-ref x 0) (vector-ref i 0))
             (* (vector-ref x 1) (vector-ref i 1))
             (* (vector-ref x 2) (vector-ref i 2))
             (* (vector-ref x 3) (vector-ref i 3))
             (vector-ref x 4) ))
          (lambda (x i)
            (+
             (* (vector-ref x 0) (vector-ref i 0))
             (* (vector-ref x 1) (vector-ref i 1))
             (* (vector-ref x 2) (vector-ref i 2))
             (* (vector-ref x 3) (vector-ref i 3))
             (* (vector-ref x 4) (vector-ref i 4))
             (vector-ref x 5) ))
          (lambda (x i)
            (+
             (* (vector-ref x 0) (vector-ref i 0))
             (* (vector-ref x 1) (vector-ref i 1))
             (* (vector-ref x 2) (vector-ref i 2))
             (* (vector-ref x 3) (vector-ref i 3))
             (* (vector-ref x 4) (vector-ref i 4))
             (* (vector-ref x 5) (vector-ref i 5))
             (vector-ref x 6) ))
          (lambda (x i)
            (+
             (* (vector-ref x 0) (vector-ref i 0))
             (* (vector-ref x 1) (vector-ref i 1))
             (* (vector-ref x 2) (vector-ref i 2))
             (* (vector-ref x 3) (vector-ref i 3))
             (* (vector-ref x 4) (vector-ref i 4))
             (* (vector-ref x 5) (vector-ref i 5))
             (* (vector-ref x 6) (vector-ref i 6))
             (vector-ref x 7) ))
          (lambda (x i)
            (+
             (* (vector-ref x 0) (vector-ref i 0))
             (* (vector-ref x 1) (vector-ref i 1))
             (* (vector-ref x 2) (vector-ref i 2))
             (* (vector-ref x 3) (vector-ref i 3))
             (* (vector-ref x 4) (vector-ref i 4))
             (* (vector-ref x 5) (vector-ref i 5))
             (* (vector-ref x 6) (vector-ref i 6))
             (* (vector-ref x 7) (vector-ref i 7))
             (vector-ref x 8) ))
          (lambda (x i)
            (+
             (* (vector-ref x 0) (vector-ref i 0))
             (* (vector-ref x 1) (vector-ref i 1))
             (* (vector-ref x 2) (vector-ref i 2))
             (* (vector-ref x 3) (vector-ref i 3))
             (* (vector-ref x 4) (vector-ref i 4))
             (* (vector-ref x 5) (vector-ref i 5))
             (* (vector-ref x 6) (vector-ref i 6))
             (* (vector-ref x 7) (vector-ref i 7))
             (* (vector-ref x 8) (vector-ref i 8))
             (vector-ref x 9) ))))
        (it
         (lambda (w)
           (lambda (x i)
             (+
              (* (vector-ref x 0) (vector-ref i 0))
              (* (vector-ref x 1) (vector-ref i 1))
              (* (vector-ref x 2) (vector-ref i 2))
              (* (vector-ref x 3) (vector-ref i 3))
              (* (vector-ref x 4) (vector-ref i 4))
              (* (vector-ref x 5) (vector-ref i 5))
              (* (vector-ref x 6) (vector-ref i 6))
              (* (vector-ref x 7) (vector-ref i 7))
              (* (vector-ref x 8) (vector-ref i 8))
              (* (vector-ref x 9) (vector-ref i 9))
              (do ((xi
                    0
                    (+
                     (* (vector-ref x u) (vector-ref i u))
                     xi ))
                   (u (- w 1) (- u 1)) )
                  ((< u 10) xi) )
              (vector-ref x w) )))))
    (lambda (r) (if (< r 10) (vector-ref em r) (funcall it r))) )))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-function array$indexer/array
    (let ((em
           (vector
            (lambda (x v i) (declare (ignore i v)) (+ (vector-ref x 0)))
            (lambda (x v i)
              (+
               (*
                (vector-ref x 0)
                (vector-ref v (array$actor-index i 0)) )
               (vector-ref x 1) ))
            (lambda (x v i)
              (+
               (*
                (vector-ref x 0)
                (vector-ref v (array$actor-index i 0)) )
               (*
                (vector-ref x 1)
                (vector-ref v (array$actor-index i 1)) )
               (vector-ref x 2) ))
            (lambda (x v i)
              (+
               (*
                (vector-ref x 0)
                (vector-ref v (array$actor-index i 0)) )
               (*
                (vector-ref x 1)
                (vector-ref v (array$actor-index i 1)) )
               (*
                (vector-ref x 2)
                (vector-ref v (array$actor-index i 2)) )
               (vector-ref x 3) ))
            (lambda (x v i)
              (+
               (*
                (vector-ref x 0)
                (vector-ref v (array$actor-index i 0)) )
               (*
                (vector-ref x 1)
                (vector-ref v (array$actor-index i 1)) )
               (*
                (vector-ref x 2)
                (vector-ref v (array$actor-index i 2)) )
               (*
                (vector-ref x 3)
                (vector-ref v (array$actor-index i 3)) )
               (vector-ref x 4) ))
            (lambda (x v i)
              (+
               (*
                (vector-ref x 0)
                (vector-ref v (array$actor-index i 0)) )
               (*
                (vector-ref x 1)
                (vector-ref v (array$actor-index i 1)) )
               (*
                (vector-ref x 2)
                (vector-ref v (array$actor-index i 2)) )
               (*
                (vector-ref x 3)
                (vector-ref v (array$actor-index i 3)) )
               (*
                (vector-ref x 4)
                (vector-ref v (array$actor-index i 4)) )
               (vector-ref x 5) ))
            (lambda (x v i)
              (+
               (*
                (vector-ref x 0)
                (vector-ref v (array$actor-index i 0)) )
               (*
                (vector-ref x 1)
                (vector-ref v (array$actor-index i 1)) )
               (*
                (vector-ref x 2)
                (vector-ref v (array$actor-index i 2)) )
               (*
                (vector-ref x 3)
                (vector-ref v (array$actor-index i 3)) )
               (*
                (vector-ref x 4)
                (vector-ref v (array$actor-index i 4)) )
               (*
                (vector-ref x 5)
                (vector-ref v (array$actor-index i 5)) )
               (vector-ref x 6) ))
            (lambda (x v i)
              (+
               (*
                (vector-ref x 0)
                (vector-ref v (array$actor-index i 0)) )
               (*
                (vector-ref x 1)
                (vector-ref v (array$actor-index i 1)) )
               (*
                (vector-ref x 2)
                (vector-ref v (array$actor-index i 2)) )
               (*
                (vector-ref x 3)
                (vector-ref v (array$actor-index i 3)) )
               (*
                (vector-ref x 4)
                (vector-ref v (array$actor-index i 4)) )
               (*
                (vector-ref x 5)
                (vector-ref v (array$actor-index i 5)) )
               (*
                (vector-ref x 6)
                (vector-ref v (array$actor-index i 6)) )
               (vector-ref x 7) ))
            (lambda (x v i)
              (+
               (*
                (vector-ref x 0)
                (vector-ref v (array$actor-index i 0)) )
               (*
                (vector-ref x 1)
                (vector-ref v (array$actor-index i 1)) )
               (*
                (vector-ref x 2)
                (vector-ref v (array$actor-index i 2)) )
               (*
                (vector-ref x 3)
                (vector-ref v (array$actor-index i 3)) )
               (*
                (vector-ref x 4)
                (vector-ref v (array$actor-index i 4)) )
               (*
                (vector-ref x 5)
                (vector-ref v (array$actor-index i 5)) )
               (*
                (vector-ref x 6)
                (vector-ref v (array$actor-index i 6)) )
               (*
                (vector-ref x 7)
                (vector-ref v (array$actor-index i 7)) )
               (vector-ref x 8) ))
            (lambda (x v i)
              (+
               (*
                (vector-ref x 0)
                (vector-ref v (array$actor-index i 0)) )
               (*
                (vector-ref x 1)
                (vector-ref v (array$actor-index i 1)) )
               (*
                (vector-ref x 2)
                (vector-ref v (array$actor-index i 2)) )
               (*
                (vector-ref x 3)
                (vector-ref v (array$actor-index i 3)) )
               (*
                (vector-ref x 4)
                (vector-ref v (array$actor-index i 4)) )
               (*
                (vector-ref x 5)
                (vector-ref v (array$actor-index i 5)) )
               (*
                (vector-ref x 6)
                (vector-ref v (array$actor-index i 6)) )
               (*
                (vector-ref x 7)
                (vector-ref v (array$actor-index i 7)) )
               (*
                (vector-ref x 8)
                (vector-ref v (array$actor-index i 8)) )
               (vector-ref x 9) ))))
          (it
           (lambda (w)
             (lambda (x v i)
               (+
                (*
                 (vector-ref x 0)
                 (vector-ref v (array$actor-index i 0)) )
                (*
                 (vector-ref x 1)
                 (vector-ref v (array$actor-index i 1)) )
                (*
                 (vector-ref x 2)
                 (vector-ref v (array$actor-index i 2)) )
                (*
                 (vector-ref x 3)
                 (vector-ref v (array$actor-index i 3)) )
                (*
                 (vector-ref x 4)
                 (vector-ref v (array$actor-index i 4)) )
                (*
                 (vector-ref x 5)
                 (vector-ref v (array$actor-index i 5)) )
                (*
                 (vector-ref x 6)
                 (vector-ref v (array$actor-index i 6)) )
                (*
                 (vector-ref x 7)
                 (vector-ref v (array$actor-index i 7)) )
                (*
                 (vector-ref x 8)
                 (vector-ref v (array$actor-index i 8)) )
                (*
                 (vector-ref x 9)
                 (vector-ref v (array$actor-index i 9)) )
                (do ((xi
                      0
                      (+
                       (*
                        (vector-ref x u)
                        (vector-ref
                         v
                         (array$actor-index i u) ))
                       xi ))
                     (u (- w 1) (- u 1)) )
                    ((< u 10) xi) )
                (vector-ref x w) )))))
      (lambda (r) (if (< r 10) (vector-ref em r) (funcall it r))) )))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-function array$applier-to-vector
    (let ((em
           (vector
            (lambda (p v) (declare (ignore v)) (funcall p))
            (lambda (p v) (funcall p (vector-ref v 0)))
            (lambda (p v)
              (funcall p (vector-ref v 0) (vector-ref v 1)) )
            (lambda (p v)
              (funcall p
                       (vector-ref v 0)
                       (vector-ref v 1)
                       (vector-ref v 2) ))
            (lambda (p v)
              (funcall p
                       (vector-ref v 0)
                       (vector-ref v 1)
                       (vector-ref v 2)
                       (vector-ref v 3) ))
            (lambda (p v)
              (funcall p
                       (vector-ref v 0)
                       (vector-ref v 1)
                       (vector-ref v 2)
                       (vector-ref v 3)
                       (vector-ref v 4) ))
            (lambda (p v)
              (funcall p
                       (vector-ref v 0)
                       (vector-ref v 1)
                       (vector-ref v 2)
                       (vector-ref v 3)
                       (vector-ref v 4)
                       (vector-ref v 5) ))
            (lambda (p v)
              (funcall p
                       (vector-ref v 0)
                       (vector-ref v 1)
                       (vector-ref v 2)
                       (vector-ref v 3)
                       (vector-ref v 4)
                       (vector-ref v 5)
                       (vector-ref v 6) ))
            (lambda (p v)
              (funcall p
                       (vector-ref v 0)
                       (vector-ref v 1)
                       (vector-ref v 2)
                       (vector-ref v 3)
                       (vector-ref v 4)
                       (vector-ref v 5)
                       (vector-ref v 6)
                       (vector-ref v 7) ))
            (lambda (p v)
              (funcall p
                       (vector-ref v 0)
                       (vector-ref v 1)
                       (vector-ref v 2)
                       (vector-ref v 3)
                       (vector-ref v 4)
                       (vector-ref v 5)
                       (vector-ref v 6)
                       (vector-ref v 7)
                       (vector-ref v 8) ))))
          (it
           (lambda (r)
             (lambda (p v)
               (apply
                p
                (vector-ref v 0)
                (vector-ref v 1)
                (vector-ref v 2)
                (vector-ref v 3)
                (vector-ref v 4)
                (vector-ref v 5)
                (vector-ref v 6)
                (vector-ref v 7)
                (vector-ref v 8)
                (vector-ref v 9)
                (do ((k r (- k 1))
                     (r
                      '()
                      (cons (vector-ref v (- k 1)) r) ))
                    ((= k 10) r) ))))))
      (lambda (r) (if (< r 10) (vector-ref em r) (funcall it r))) )))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-function array$applier-to-actor
    (let ((em
           (vector
            (lambda (p a) (declare (ignore a)) (funcall p))
            (lambda (p a) (funcall p (array-ref a 0)))
            (lambda (p a)
              (funcall p (array-ref a 0) (array-ref a 1)) )
            (lambda (p a)
              (funcall p
                       (array-ref a 0)
                       (array-ref a 1)
                       (array-ref a 2) ))
            (lambda (p a)
              (funcall p
                       (array-ref a 0)
                       (array-ref a 1)
                       (array-ref a 2)
                       (array-ref a 3) ))
            (lambda (p a)
              (funcall p
                       (array-ref a 0)
                       (array-ref a 1)
                       (array-ref a 2)
                       (array-ref a 3)
                       (array-ref a 4) ))
            (lambda (p a)
              (funcall p
                       (array-ref a 0)
                       (array-ref a 1)
                       (array-ref a 2)
                       (array-ref a 3)
                       (array-ref a 4)
                       (array-ref a 5) ))
            (lambda (p a)
              (funcall p
                       (array-ref a 0)
                       (array-ref a 1)
                       (array-ref a 2)
                       (array-ref a 3)
                       (array-ref a 4)
                       (array-ref a 5)
                       (array-ref a 6) ))
            (lambda (p a)
              (funcall p
                       (array-ref a 0)
                       (array-ref a 1)
                       (array-ref a 2)
                       (array-ref a 3)
                       (array-ref a 4)
                       (array-ref a 5)
                       (array-ref a 6)
                       (array-ref a 7) ))
            (lambda (p a)
              (funcall p
                       (array-ref a 0)
                       (array-ref a 1)
                       (array-ref a 2)
                       (array-ref a 3)
                       (array-ref a 4)
                       (array-ref a 5)
                       (array-ref a 6)
                       (array-ref a 7)
                       (array-ref a 8) ))))
          (it
           (lambda (r)
             (lambda (p a)
               (declare (ignore p))
               (apply a
                      (array-ref a 0)
                      (array-ref a 1)
                      (array-ref a 2)
                      (array-ref a 3)
                      (array-ref a 4)
                      (array-ref a 5)
                      (array-ref a 6)
                      (array-ref a 7)
                      (array-ref a 8)
                      (array-ref a 9)
                      (do ((k r (- k 1))
                           (r '() (cons (array-ref a (- k 1)) r)) )
                          ((= k 10) r) ))))))
      (lambda (r)
        "These are high level, hiding implementation at call site."
        (if (< r 10) (vector-ref em r) (funcall it r)) ))))
(define-function array$applier-to-backing-vector
  (let ((em
         (vector
          (lambda (p ai av) (declare (ignore ai av)) (funcall p))
          (lambda (p ai av)
            (funcall p (vector-ref av (array$actor-index ai 0))) )
          (lambda (p ai av)
            (funcall p
             (vector-ref av (array$actor-index ai 0))
             (vector-ref av (array$actor-index ai 1)) ))
          (lambda (p ai av)
            (funcall p
             (vector-ref av (array$actor-index ai 0))
             (vector-ref av (array$actor-index ai 1))
             (vector-ref av (array$actor-index ai 2)) ))
          (lambda (p ai av)
            (funcall p
             (vector-ref av (array$actor-index ai 0))
             (vector-ref av (array$actor-index ai 1))
             (vector-ref av (array$actor-index ai 2))
             (vector-ref av (array$actor-index ai 3)) ))
          (lambda (p ai av)
            (funcall p
             (vector-ref av (array$actor-index ai 0))
             (vector-ref av (array$actor-index ai 1))
             (vector-ref av (array$actor-index ai 2))
             (vector-ref av (array$actor-index ai 3))
             (vector-ref av (array$actor-index ai 4)) ))
          (lambda (p ai av)
            (funcall p
             (vector-ref av (array$actor-index ai 0))
             (vector-ref av (array$actor-index ai 1))
             (vector-ref av (array$actor-index ai 2))
             (vector-ref av (array$actor-index ai 3))
             (vector-ref av (array$actor-index ai 4))
             (vector-ref av (array$actor-index ai 5)) ))
          (lambda (p ai av)
            (funcall p
             (vector-ref av (array$actor-index ai 0))
             (vector-ref av (array$actor-index ai 1))
             (vector-ref av (array$actor-index ai 2))
             (vector-ref av (array$actor-index ai 3))
             (vector-ref av (array$actor-index ai 4))
             (vector-ref av (array$actor-index ai 5))
             (vector-ref av (array$actor-index ai 6)) ))
          (lambda (p ai av)
            (funcall p
             (vector-ref av (array$actor-index ai 0))
             (vector-ref av (array$actor-index ai 1))
             (vector-ref av (array$actor-index ai 2))
             (vector-ref av (array$actor-index ai 3))
             (vector-ref av (array$actor-index ai 4))
             (vector-ref av (array$actor-index ai 5))
             (vector-ref av (array$actor-index ai 6))
             (vector-ref av (array$actor-index ai 7)) ))
          (lambda (p ai av)
            (funcall p
             (vector-ref av (array$actor-index ai 0))
             (vector-ref av (array$actor-index ai 1))
             (vector-ref av (array$actor-index ai 2))
             (vector-ref av (array$actor-index ai 3))
             (vector-ref av (array$actor-index ai 4))
             (vector-ref av (array$actor-index ai 5))
             (vector-ref av (array$actor-index ai 6))
             (vector-ref av (array$actor-index ai 7))
             (vector-ref av (array$actor-index ai 8)) ))))
        (it
         (lambda (r)
           (lambda (p ai av)
             (apply
              p
              (vector-ref av (array$actor-index ai 0))
              (vector-ref av (array$actor-index ai 1))
              (vector-ref av (array$actor-index ai 2))
              (vector-ref av (array$actor-index ai 3))
              (vector-ref av (array$actor-index ai 4))
              (vector-ref av (array$actor-index ai 5))
              (vector-ref av (array$actor-index ai 6))
              (vector-ref av (array$actor-index ai 7))
              (vector-ref av (array$actor-index ai 8))
              (vector-ref av (array$actor-index ai 9))
              (do ((k r (- k 1))
                   (r
                    '()
                    (cons
                     (vector-ref
                      av
                      (array$actor-index ai (- k 1)) )
                     r )))
                  ((= k 10) r) ))))))
    (lambda (r)
      "These are low level, exposing implementation at call site."
      (if (< r 10) (vector-ref em r) (funcall it r)) )))
(define-function (array$index/vector r x v)
  (funcall (array$indexer/vector r) x v) )
(define-function (array$index/array r x av ai)
  (funcall (array$indexer/array r) x av ai) )
(define-function (array$apply-to-vector r p v)
  (funcall (array$applier-to-vector r) p v))
(define-function (array$apply-to-actor r p a)
  (funcall (array$applier-to-actor r) p a))
;;; array
;;; 1997 - 2001 Jussi Piitulainen

;;; --- Intro ---

;;; This interface to arrays is based on Alan Bawden's array.scm of
;;; 1993 (earlier version in the Internet Repository and another
;;; version in SLIB). This is a complete rewrite, to be consistent
;;; with the rest of Scheme and to make arrays independent of lists.

;;; Some modifications are due to discussion in srfi-25 mailing list.

;;; (array? obj)
;;; (make-array shape [obj])             changed arguments
;;; (shape bound $.)                    new
;;; (array shape obj ...)                new
;;; (array-rank array)                   changed name back
;;; (array-start array dimension)        new
;;; (array-end array dimension)          new
;;; (array-ref array k ...)
;;; (array-ref array index)              new variant
;;; (array-set! array k ... obj)         changed argument order
;;; (array-set! array index obj)         new variant
;;; (share-array array shape proc)       changed arguments

;;; All other variables in this file have names in "array..".

;;; Should there be a way to make arrays with initial values mapped
;;; from indices? Sure. The current "initial object" is lame.
;;;
;;; Removed (array-shape array) from here. There is a new version
;;; in arlib though.

;;; --- Representation type dependencies ---

;;; The mapping from array indices to the index to the underlying vector
;;; is whatever array..optimize returns. The file "opt" provides three
;;; representations:
;;;
;;; mbda) mapping is a procedure that allows an optional argument
;;; tter) mapping is two procedures that takes exactly the indices
;;; ctor) mapping is a vector of a constant term and coefficients
;;;
;;; Choose one in "opt" to make the optimizer. Then choose the matching
;;; implementation of array-ref and array-set!.
;;;
;;; These should be made macros to inline them. Or have a good compiler
;;; and plant the package as a module.

;;; 1. Pick an optimizer.
;;; 2. Pick matching index representation.
;;; 3. Pick a record implementation; as-procedure is generic; syntax inlines.
;;; 3. This file is otherwise portable.

;;; --- Portable R5RS (R4RS and multiple values) ---

;;; (array? obj)
;;; returns T if `obj' is an array and T or 'nil otherwise.

(define-function (array? obj)
  (array$array? obj))

;;; (make-array shape)
;;; (make-array shape obj)
;;; makes array of `shape' with each cell containing `obj' initially.

(define-function (make-array shape . rest)
  (or (array$good-shape? shape)
      (error "make-array: shape is not a shape"))
  (apply #'array$make-array shape rest))

(define-function (array$make-array shape . rest)
  (let ((size (array$size shape)))
    (array$make
     (if (pair? rest)
         (apply (lambda (o) (make-vector size o)) rest)
         (make-vector size))
     (if (= size 0)
         (array$optimize-empty
          (vector-ref (array$shape shape) 1))
         (array$optimize
          (array$make-index shape)
          (vector-ref (array$shape shape) 1)))
     (array$shape->vector shape))))

;;; (shape bound $.)
;;; makes a shape. Bounds must be an even number of exact, pairwise
;;; non-decreasing integers. Note that any such array can be a shape.

(define-function (shape . bounds)
  (let ((v (list->vector bounds)))
    (or (even? (vector-length v))
        (error (string-append "shape: uneven number of bounds: "
                              (array$list->string bounds))))
    (let ((shp (array$make
                v
                (if (pair? bounds)
                    (array$shape-index)
                    (array$empty-shape-index))
                (vector 0 (quotient (vector-length v) 2)
                        0 2))))
      (or (array$good-shape? shp)
          (error (string-append "shape: bounds are not pairwise "
                                "non-decreasing exact integers: "
                                (array$list->string bounds))))
      shp)))

;;; (array shape obj ...)
;;; is analogous to `vector'.

(define-function (array shape . elts)
  (or (array$good-shape? shape)
      (error (string-append "array: shape " (array$thing->string shape)
                            " is not a shape")))
  (let ((size (array$size shape)))
    (let ((vector (list->vector elts)))
      (or (= (vector-length vector) size)
          (error (string-append "array: an array of shape "
                                (array$shape-vector->string
                                 (array$vector shape))
                                " has "
                                (number->string size)
                                " elements but got "
                                (number->string (vector-length vector))
                                " values: "
                                (array$list->string elts))))
      (array$make
       vector
       (if (= size 0)
           (array$optimize-empty
            (vector-ref (array$shape shape) 1))
           (array$optimize
            (array$make-index shape)
            (vector-ref (array$shape shape) 1)))
       (array$shape->vector shape)))))

;;; (array-rank array)
;;; returns the number of dimensions of `array'.

(define-function (array-rank array)
  (quotient (vector-length (array$shape array)) 2))

;;; (array-start array k)
;;; returns the lower bound index of array along dimension k. This is
;;; the least valid index along that dimension if the dimension is not
;;; empty.

(define-function (array-start array d)
  (vector-ref (array$shape array) (+ d d)))

;;; (array-end array k)
;;; returns the upper bound index of array along dimension k. This is
;;; not a valid index. If the dimension is empty, this is the same as
;;; the lower bound along it.

(define-function (array-end array d)
  (vector-ref (array$shape array) (+ d d 1)))

;;; (share-array array shape proc)
;;; makes an array that shares elements of `array' at shape `shape'.
;;; The arguments to `proc' are indices of the result.  The values of
;;; `proc' are indices of `array'.

;;; Todo: in the error message, should recognise the mapping and show it.

(define-function (share-array array subshape f)
  (or (array$good-shape? subshape)
      (error (string-append "share-array: shape "
                            (array$thing->string subshape)
                            " is not a shape")))
  (let ((subsize (array$size subshape)))
    (or (array$good-share? subshape subsize f (array$shape array))
        (error (string-append "share-array: subshape "
                              (array$shape-vector->string
                               (array$vector subshape))
                              " does not map into supershape "
                              (array$shape-vector->string
                               (array$shape array))
                              " under mapping "
                              (array$map->string
                               f
                               (vector-ref (array$shape subshape) 1)))))
    (let ((g (array$index array)))
      (array$make
       (array$vector array)
       (if (= subsize 0)
           (array$optimize-empty
            (vector-ref (array$shape subshape) 1))
           (array$optimize
            (lambda ks
              (multiple-value-call
               (lambda ks (array$vector-index g ks))
               (apply f ks)))
            (vector-ref (array$shape subshape) 1)))
       (array$shape->vector subshape)))))

;;; --- Hrmph ---

;;; (array$share/index! ...)
;;; reuses a user supplied index object when recognising the
;;; mapping. The mind balks at the very nasty side effect that
;;; exposes the implementation. So this is not in the spec.
;;; But letting index objects in at all creates a pressure
;;; to go the whole hog. Arf.

;;; Use array$optimize-empty for an empty array to get a
;;; clearly invalid vector index.

;;; Surely it's perverse to use an actor for index here? But
;;; the possibility is provided for completeness.

(define-function (array$share/index! array subshape proc index)
  (array$make
   (array$vector array)
   (if (= (array$size subshape) 0)
       (array$optimize-empty
        (quotient (vector-length (array$shape array)) 2))
       (funcall (if (vector? index)
                    #'array$optimize/vector
                    #'array$optimize/actor)
        (lambda (subindex)
          (let ((superindex (funcall proc subindex)))
            (if (vector? superindex)
                (array$index/vector
                 (quotient (vector-length (array$shape array)) 2)
                 (array$index array)
                 superindex)
                (array$index/array
                 (quotient (vector-length (array$shape array)) 2)
                 (array$index array)
                 (array$vector superindex)
                 (array$index superindex)))))
        index))
   (array$shape->vector subshape)))

(define-function (array$optimize/vector f v)
  (let ((r (vector-length v)))
    (do ((k 0 (+ k 1)))
      ((= k r))
      (vector-set! v k 0))
    (let ((n0 (funcall f v))
          (cs (make-vector (+ r 1)))
          (apply (array$applier-to-vector (+ r 1))))
      (vector-set! cs 0 n0)
      (let wok ((k 0))
        (if (< k r)
            (let ((k1 (+ k 1)))
              (vector-set! v k 1)
              (let ((nk (- (funcall f v) n0)))
                (vector-set! v k 0)
                (vector-set! cs k1 nk)
                (wok k1)))))
      (funcall apply (array$maker r) cs))))

(define-function (array$optimize/actor f a)
  (let ((r (array-end a 0))
        (v (array$vector a))
        (i (array$index a)))
    (do ((k 0 (+ k 1)))
      ((= k r))
      (vector-set! v (array$actor-index i k) 0))
    (let ((n0 (funcall f a))
          (cs (make-vector (+ r 1)))
          (apply (array$applier-to-vector (+ r 1))))
      (vector-set! cs 0 n0)
      (let wok ((k 0))
        (if (< k r)
            (let ((k1 (+ k 1))
                  (tee (array$actor-index i k)))
              (vector-set! v tee 1)
              (let ((nk (- (funcall f a) n0)))
                (vector-set! v tee 0)
                (vector-set! cs k1 nk)
                (wok k1)))))
      (funcall apply (array$maker r) cs))))

;;; --- Internals ---

(define-function (array$shape->vector shape)
  (let ((idx (array$index shape))
        (shv (array$vector shape))
        (rnk (vector-ref (array$shape shape) 1)))
    (let ((vec (make-vector (* rnk 2))))
      (do ((k 0 (+ k 1)))
        ((= k rnk)
         vec)
        (vector-set! vec (+ k k)
                     (vector-ref shv (array$shape-vector-index idx k 0)))
        (vector-set! vec (+ k k 1)
                     (vector-ref shv (array$shape-vector-index idx k 1)))))))

;;; (array$size shape)
;;; returns the number of elements in arrays of shape `shape'.

(define-function (array$size shape)
   (let ((idx (array$index shape))
         (shv (array$vector shape))
         (rnk (vector-ref (array$shape shape) 1)))
     (do   ((k 0 (+ k 1))
            (s 1 (* s
                    (- (vector-ref shv (array$shape-vector-index idx k 1))
                       (vector-ref shv (array$shape-vector-index idx k 0))))))
       ((= k rnk) s))))

;;; (array$make-index shape)
;;; returns an index function for arrays of shape `shape'. This is a
;;; runtime composition of several variable arity procedures, to be
;;; passed to array$optimize for recognition as an affine function of
;;; as many variables as there are dimensions in arrays of this shape.

(define-function (array$make-index shape)
  (let ((idx (array$index shape))
        (shv (array$vector shape))
        (rnk (vector-ref (array$shape shape) 1)) )
    (do ((f (constantly 0)
           (lambda (k . ks)
                (+ (* s (- k (vector-ref
                              shv
                              (array$shape-vector-index idx (- j 1) 0)
                              )))
                   (apply f ks))))
         (s 1 (* s (- (vector-ref
                       shv
                       (array$shape-vector-index idx (- j 1) 1) )
                      (vector-ref
                       shv
                       (array$shape-vector-index idx (- j 1) 0) ))))
         (j rnk (let ((j j))
                  (- j 1)) ))
        ((= j 0)
         f ))))

;;; --- Error checking ---

;;; (array$good-shape? shape)
;;; returns true if `shape' is an array of the right shape and its
;;; elements are exact integers that pairwise bound intervals `[lo$hi).

(define-function (array$good-shape? shape)
  (and (array$array? shape)
       (let ((u (array$shape shape))
             (v (array$vector shape))
             (x (array$index shape)))
         (and (= (vector-length u) 4)
              (= (vector-ref u 0) 0)
              (= (vector-ref u 2) 0)
              (= (vector-ref u 3) 2))
         (let ((p (vector-ref u 1)))
           (do ((k 0 (+ k 1))
                (true T (let ((lo (vector-ref
                                    v
                                    (array$shape-vector-index x k 0)))
                               (hi (vector-ref
                                    v
                                    (array$shape-vector-index x k 1))))
                           (and true
                                (integer? lo)
                                (exact? lo)
                                (integer? hi)
                                (exact? hi)
                                (<= lo hi)))))
             ((= k p) true))))))

;;; (array$good-share? subv subsize mapping superv)
;;; returns true if the extreme indices in the subshape vector map
;;; into the bounds in the supershape vector.

;;; If some interval in `subv' is empty, then `subv' is empty and its
;;; image under `f' is empty and it is trivially alright.  One must
;;; not call `f', though.

(define-function (array$good-share? subshape subsize f super)
  (or (zero? subsize)
      (letrec
          ((sub (array$vector subshape))
           (dex (array$index subshape))
           (ck (lambda (k ks)
		 (if (zero? k)
                     (multiple-value-call
                      (lambda qs (array$good-indices? qs super))
                      (apply f ks))
                     (and (ck (- k 1)
                              (cons (vector-ref
                                     sub
                                     (array$shape-vector-index
                                      dex
                                      (- k 1)
                                      0))
                                    ks))
                          (ck (- k 1)
                              (cons (- (vector-ref
                                        sub
                                        (array$shape-vector-index
                                         dex
                                         (- k 1)
                                         1))
                                       1)
                                    ks)))))))
        (let ((rnk (vector-ref (array$shape subshape) 1)))
          (or (array$unchecked-share-depth? rnk)
              (ck rnk '()))))))

;;; Check good-share on 10 dimensions at most. The trouble is,
;;; the cost of this check is exponential in the number of dimensions.

(define-function (array$unchecked-share-depth? rank)
  (if (> rank 10)
      (begin
        (display `(warning\: unchecked depth in share\:
                            ,rank subdimensions))
        (newline)
        T)
      'nil))

;;; (array$check-indices caller indices shape-vector)
;;; (array$check-indices.o caller indices shape-vector)
;;; (array$check-index-vector caller index-vector shape-vector)
;;; return if the index is in bounds, else signal error.
;;;
;;; Shape-vector is the internal representation, with
;;; b and e for dimension k at 2k and 2k + 1.

(define-function (array$check-indices who ks shv)
  (or (array$good-indices? ks shv)
      (error (array$not-in who ks shv))))

(define-function (array$check-indices.o who ks shv)
  (or (array$good-indices.o? ks shv)
      (error (array$not-in who (reverse (cdr (reverse ks))) shv))))

(define-function (array$check-index-vector who ks shv)
  (or (array$good-index-vector? ks shv)
      (error (array$not-in who (vector->list ks) shv))))

(define-function (array$check-index-actor who ks shv)
  (let ((shape (array$shape ks)))
    (or (and (= (vector-length shape) 2)
             (= (vector-ref shape 0) 0))
        (error "not an actor"))
    (or (array$good-index-actor?
         (vector-ref shape 1)
         (array$vector ks)
         (array$index ks)
         shv)
        (array$not-in who (do ((k (vector-ref shape 1) (- k 1))
                               (m '() (cons (vector-ref
                                             (array$vector ks)
                                             (array$actor-index
                                              (array$index ks)
                                              (- k 1)))
                                            m)))
                            ((= k 0) m))
                      shv))))

(define-function (array$good-indices? ks shv)
   (let ((d2 (vector-length shv)))
      (do ((kp ks (if (pair? kp)
                      (cdr kp)))
           (k 0 (+ k 2))
           (true T (and true (pair? kp)
                         (array$good-index? (car kp) shv k))))
        ((= k d2)
         (and true (null? kp))))))

(define-function (array$good-indices.o? ks.o shv)
   (let ((d2 (vector-length shv)))
     (do   ((kp ks.o (if (pair? kp)
                         (cdr kp)))
            (k 0 (+ k 2))
            (true T (and true (pair? kp)
                          (array$good-index? (car kp) shv k))))
       ((= k d2)
        (and true (pair? kp) (null? (cdr kp)))))))

(define-function (array$good-index-vector? ks shv)
  (let ((r2 (vector-length shv)))
    (and (= (* 2 (vector-length ks)) r2)
         (do ((j 0 (+ j 1))
              (k 0 (+ k 2))
              (true T (and true
                            (array$good-index? (vector-ref ks j) shv k))))
           ((= k r2) true)))))

(define-function (array$good-index-actor? r v i shv)
  (and (= (* 2 r) (vector-length shv))
       (do ((j 0 (+ j 1))
            (k 0 (+ k 2))
            (true T (and true
                          (array$good-index? (vector-ref
                                              v
                                              (array$actor-index i j))
                                             shv
                                             k))))
         ((= j r) true))))

;;; (array$good-index? index shape-vector 2d)
;;; returns true if index is within bounds for dimension 2d/2.

(define-function (array$good-index? w shv k)
  (and (integer? w)
       (exact? w)
       (<= (vector-ref shv k) w)
       (< w (vector-ref shv (+ k 1)))))

(define-function (array$not-in who ks shv)
  (let ((index (array$list->string ks))
        (bounds (array$shape-vector->string shv)))
    (error (string-append who
                          ": index " index
                          " not in bounds " bounds))))

(define-function (array$list->string ks)
  (do ((index "" (string-append index (array$thing->string (car ks)) " "))
       (ks ks (cdr ks)))
    ((null? ks) index)))

(define-function (array$shape-vector->string shv)
  (do ((bounds "" (string-append bounds
                                 "["
                                 (number->string (vector-ref shv tee))
                                 ".."
                                 (number->string (vector-ref shv (+ tee 1)))
                                 ")"
                                 " "))
       (tee 0 (+ tee 2)))
    ((= tee (vector-length shv)) bounds)))

(define-function (array$thing->string thing)
  (typecase thing
    (null "()")
    (number (number->string thing))
    (symbol (string-append "#<symbol>" (string thing)))
    (character "#<char>")
    (string "#<string>")
    (cons (string-append "#" (number->string (length thing))
                         "<list>"))
    (vector (string-append "#" (number->string
                                (vector-length thing))
                           "<vector>"))
    (cl:array "#<array>")
    (function "#<procedure>")
    (otherwise (case thing
                 ((T) "T")
                 (otherwise
                  "#<whatsit>")))))

;;; And to grok an affine map, vector->vector type. Column k of arr
;;; will contain coefficients n0 ... nm of 1 k1 ... km for kth value.
;;;
;;; These are for the error message when share fails.

(define-function (array$index-ref ind k)
  (if (vector? ind)
      (vector-ref ind k)
      (vector-ref
       (array$vector ind)
       (array$actor-index (array$index ind) k))))

(define-function (array$index-set! ind k o)
  (if (vector? ind)
      (vector-set! ind k o)
      (vector-set!
       (array$vector ind)
       (array$actor-index (array$index ind) k)
       o)))

(define-function (array$index-length ind)
  (if (vector? ind)
      (vector-length ind)
      (vector-ref (array$shape ind) 1)))

(define-function (array$map->string proc r)
  (let* ((m (array$grok/arguments proc r))
         (s (vector-ref (array$shape m) 3)))
    (do ((i "" (string-append i c "k" (number->string k)))
         (c "" ", ")
         (k 1 (+ k 1)))
      ((< r k)
       (do ((o "" (string-append o c (array$map-column->string m r k)))
            (c "" ", ")
            (k 0 (+ k 1)))
         ((= k s)
          (string-append i " => " o)))))))

(define-function (array$map-column->string m r k)
  (let ((v (array$vector m))
        (i (array$index m)))
    (let ((n0 (vector-ref v (array$vector-index i (list 0 k)))))
      (let wok ((j 1)
                (e (if (= n0 0) "" (number->string n0))))
        (if (<= j r)
            (let ((nj (vector-ref v (array$vector-index i (list j k)))))
              (if (= nj 0)
                  (wok (+ j 1) e)
                  (let* ((nj (if (= nj 1) ""
                                 (if (= nj -1) "-"
                                     (string-append (number->string nj)
                                                    " "))))
                         (njkj (string-append nj "k" (number->string j))))
                    (if (string= e "")
                        (wok (+ j 1) njkj)
                        (wok (+ j 1) (string-append e " + " njkj))))))
            (if (string= e "") "0" e))))))

(define-function (array$grok/arguments proc r)
  (array$grok/index!
   (lambda (vec)
     (multiple-value-call #'vector
                          (array$apply-to-vector r proc vec)))
   (make-vector r)))

(define-function (array$grok/index! proc in)
  (let ((m (array$index-length in)))
    (do ((k 0 (+ k 1)))
        ((= k m))
      (array$index-set! in k 0) )
    (let* ((n0 (funcall proc in))
           (n (array$index-length n0)) )
      (let ((arr (make-array (shape 0 (+ m 1) 0 n))))  ; (*)
        (do ((k 0 (+ k 1)))
            ((= k n))
          (array-set! arr 0 k (array$index-ref n0 k)) ) ; (**)
        (do ((j 0 (+ j 1)))
            ((= j m))
          (array$index-set! in j 1)
          (let ((nj (funcall proc in)))
            (array$index-set! in j 0)
            (do ((k 0 (+ k 1)))
                ((= k n))
              (array-set! arr (+ j 1) k (- (array$index-ref nj k) ; (**)
                                           (array$index-ref n0 k) )))))
        arr ))))
;; (*)  Should not use `make-array' and `shape' here
;; (**) Should not use `array-set!' here
;; Should use something internal to the library instead: either lower
;; level code (preferable but complex) or alternative names to these same.
