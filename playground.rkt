#lang racket
(define (myloop)
  (let ((a (read))
        (b (read)))
    (if (eq? a eof)
        (void)
        (begin (displayln (+ a b)) (myloop)))))

(define (square x)
  (define (double x) (+ x x))
  (exp (double (log x))))

(define (solve-coin n coins)
  (cond ((< n 0) 0)
        ((= n 0) 1)
        ((equal? coins '()) 0)
        (else
         (+ (solve-coin (- n (car coins)) coins)
            (solve-coin n (cdr coins))))))

(define (f n)
  (define (f-iter x y z n)
    (if (< n 3)
        x
        (f-iter (+ (* 3 z) (* 2 y) x) x y (- n 1))))
  (f-iter 2 1 0 n))

(define (f-recur n)
  (cond ((< n 3) n)
        (else (+ (f-recur (- n 1))
                 (* 2 (f-recur (- n 2)))
                 (* 3 (f-recur (- n 3)))))))

(define (pascal row col)
  (cond ((or (= row col) (= col 1)) 1)
        (else (+ (pascal (- row 1) col)
                 (pascal (- row 1) (- col 1))))))

(define (sqr x) (* x x))

(define (expt b n)
  (cond ((= n 0) 1)
        ((even? n)
         (sqr (expt b (/ n 2))))
        (else (* b (expt b (- n 1))))))

(define (expt-fast b n)
  (define (expt-iter b n a)
    (cond ((= n 0) a)
          ((even? n)
           (expt-iter (sqr b) (/ n 2) a))
          (else
           (expt-iter b (- n 1) (* a b)))))
  (expt-iter b n 1))

(define (expt-naive b n)
  (if (= n 0)
      1
      (* b (expt-naive b (- n 1)))))

(define (mult a b)
  (define (double a) (* 2 a))
  (define (halve a) (/ a 2))
  (define (mult-iter a b k)
    (cond ((= b 1) a)
          ((even? b)
           (mult-iter (double a) (halve b) k))
          (else
           (mult-iter a (- b 1) (+ k a)))))
  (mult-iter a b 0))

; (a, b) -> ((bq + aq + ap), (aq + bp))
; -> (b(2pq + q^2) + a(2pq + q^2) + a(p^2 + q^2), a(2pq + q^2) + b(p^2 + q^2))
(define (fib n)
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-iter a b
                     (+ (sqr p) (sqr q))
                     (+ (* 2 p q) (sqr q))
                     (/ count 2)))
          (else
           (fib-iter (+ (* b q) (* a q) (* a p))
                     (+ (* b p) (* a q))
                     p q (- count 1)))))
  (fib-iter 1 0 0 1 n))

(define (fib-naive n)
  (if (<= n 2)
      1
      (+ (fib-naive (- n 1))
         (fib-naive (- n 2)))))