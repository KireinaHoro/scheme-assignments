#lang racket

(include "primality.rkt")

(define (myloop)
  (let ((a (read))
        (b (read)))
    (if (eq? a eof)
        (void)
        (begin (displayln (+ a b)) (myloop)))))

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

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpson-integral f a b n)
  (let ((h (/ (- b a) n)))
    (define (next x) (+ x h))
    (define term
      (lambda (i) (lambda (x)
                    (if (even? i) (* 2 (f x))
                        (* 4 (f x))))))
    (define (sum* term a next b counter)
      (if (> a b)
          0
          (+ ((term counter) a)
             (sum* term (next a) next b (+ counter 1)))))
    (* (/ h 3)
       (+ (f a) (f b)
          (sum* term (+ a h) next (- b h) 1)))))

(define (product term a next b)
  (define (iter curr result)
    (if (> curr b)
        result
        (iter (next curr) (* result (term curr)))))
  (iter a 1))

(define (fac n)
  (product (lambda (x) x) 1 (lambda (x) (+ 1 x)) n))

(define (pi-product last-elem)
  (* 2
     (product (lambda (x) (square (/ (+ x 1) x)))
              3 (lambda (x) (+ x 2)) last-elem)
     (/ 1 (- last-elem 1))))

(define (accumulate combiner null-value term a next b)
  (define (iter curr result)
    (if (> curr b)
        result
        (iter (next curr) (combiner result (term curr)))))
  (iter a null-value))

(define (accumulate-recur combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-recur combiner null-value term (next a) next b))))

(define (filtered-accumulate combiner null-value term a next b predicate)
  (if (> a b)
      null-value
      (let ((rest (filtered-accumulate combiner null-value term (next a) next b predicate))
            (curr (term a)))
        (if (predicate a)   ; test on the current a
            (combiner curr rest)
            rest))))

(define (f1 a b)   ; the sum of the squares of the prime numbers in the interval a to b
  (filtered-accumulate + 0 square a (lambda (x) (+ x 1)) b naive-prime?))

(define (f2 n)     ; the product of all the positive integers less than n that are
                   ; relatively prime to n (i.e., all positive integers i < n such that GCD(i,n) = 1).
  (define (pred? t)
    (= 1 (gcd n t)))
  (filtered-accumulate * 1 (lambda (x) x) 1 (lambda (x) (+ x 1)) n pred?))
