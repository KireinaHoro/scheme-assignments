#lang racket

(include "primality.rkt")

(define (myloop)
  (let ((a (read))
        (b (read)))
    (if (eq? a eof)
        (void)
        (begin (displayln (+ a b)) (myloop)))))

(define (solve-coin n coins)
  (cond ((negative? n) 0)
        ((zero? n) 1)
        ((equal? coins '()) 0)
        (else
         (+ (solve-coin (- n (car coins)) coins)
            (solve-coin n (cdr coins))))))

(define (f0 n)
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

(define (my-expt b n)
  (cond ((zero? n) 1)
        ((even? n)
         (sqr (my-expt b (/ n 2))))
        (else (* b (my-expt b (- n 1))))))

(define (expt-fast b n)
  (define (expt-iter b n a)
    (cond ((zero? n) a)
          ((even? n)
           (expt-iter (sqr b) (/ n 2) a))
          (else
           (expt-iter b (- n 1) (* a b)))))
  (expt-iter b n 1))

(define (expt-naive b n)
  (if (zero? n)
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

; Ex1.30
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

; Ex1.29
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

; Ex1.31
(define (product term a next b)
  (define (iter curr result)
    (if (> curr b)
        result
        (iter (next curr) (* result (term curr)))))
  (iter a 1))

(define (fac n)
  (product (lambda (x) x) 1 (lambda (x) (+ 1 x)) n))

; Ex1.32
(define (pi-product last-elem)
  (* 2
     (product (lambda (x) (square (/ (+ x 1) x)))
              3 (lambda (x) (+ x 2)) last-elem)
     (/ 1 (- last-elem 1))))

; Ex1.33
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

; half interval method for finding the root of given function
(define (search f neg-point pos-point)
  (define (close-enough? x y)
    (< (abs (- x y)) 1e-10))
  (define (average x y) (/ (+ x y) 2))
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

; wrapper around search to ensure that a and b satisfy the sign constraints
(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (positive? a-value) (negative? b-value))
           (search f b a))
          ((zero? a-value) a)
          ((zero? b-value) b)
          (else
           (error "Values are not of opposite sign on" a b)))))

; calculate the fixed-point of f
; test with: (fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
; golden ratio: fixed point of x |-> 1 + 1/x
(define (fixed-point f first-guess)
  (define (average x y) (/ (+ x y) 2))
  (define (close-enough? x y)
    (< (abs (- x y)) 1e-10))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          ;(try (average next guess))  ; with explicit average damping
          (try next))))                ; without
  (try first-guess))

; Ex1.36
; fixed-point with debug messages
(define (fixed-point-print f first-guess)
  (define (average x y) (/ (+ x y) 2))
  (define (close-enough? x y)
    (< (abs (- x y)) 1e-10))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (begin (display "trying ") (display next) (newline)
                 ;(try (average next guess))  ; with explicit average damping
                 (try next)                 ; without
                 ))))
  (try first-guess))

; Ex1.37
; compute k-term finite continued fraction with N and D denoted by unary functions n and d
(define (cont-frac n d k)
  (if (zero? k) 0
      (let ((new-n (lambda (x) (n (+ x 1))))
            (new-d (lambda (x) (d (+ x 1)))))
        (/ (n 1)
           (+ (d 1) (cont-frac new-n new-d (- k 1)))))))

; iterative version of cont-frac
; calculation from bottom up (i.e. starting from k) is needed
; test with: substitude cont-frac below with cont-frac-iter (at * mark)
(define (cont-frac-iter n d k)
  (define (iter curr result)
    (if (= 1 curr)    ; stop at N1 as we picked curr - 1 later on
        result
        (let ((next (- curr 1)))
          (iter next
                (/ (n next)
                   (+ (d next) result))))))
  (iter k
        (/ (n k) (d k))))

; print out the results of k-term finite fraction results when calculating the
; golden ratio with cont-frac
; test with: (seq 20) (expected precision: 1e-7)
(define (seq k)
  (define (precision k)
    (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k))   ; *
  (define (iter curr)
    (if (> curr k) (void)
        (let ((res (precision curr)))
          (begin (display curr) (display "-term fraction result: ") (displayln res)
                 (iter (+ curr 1))))))
  (iter 1))

; Ex1.38
; calculate e according to continual fraction expansion from Leonhard Euler
; n: 1 1 1 1 1 1 1 1 1 1 1 ...
; d: 1 2 1 1 4 1 1 6 1 1 8 ...
(define (e n)
  (+ 2 (cont-frac-iter (lambda (_) 1.0)
                       (lambda (x)
                         (match (remainder x 3)
                           (2 (* 2 (+ 1 (floor (/ x 3)))))
                           (_ 1)))
                       n)))

; Ex1.39
; calculate tangent with continued fraction from J.H. Lambert
; calculate -x*tan(x) first
(define (tan-cf x k)
  (define result
    (cont-frac-iter (lambda (_) (- (square x)))
                    (lambda (t) (- (* 2 t) 1))
                    k))
  (if (zero? x) 0
      (- (/ result x))))

; take the average of the variable and the function value
(define (average-damp f)
  (define (average x y) (/ (+ x y) 2))
  (lambda (x) (average x (f x))))

(define (my-sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

; derivative of g: (g(x + dx) - g(x)) / dx
(define (deriv g)
  (define dx 1e-10)
  (lambda (x) (/ (- (g (+ x dx))
                    (g x))
                 dx)))

; verbosely calculate
(define (newton-method g guess)
  (define (newton-transform g)   ; f(x) = x - g(x) / g'(x) (take intersection of tangent with y=0)
    (lambda (x)
      (- x (/ (g x) ((deriv g) x)))))
  (fixed-point-print (average-damp (newton-transform g)) guess))

; calculate fixed point of the transformed function
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (fixed-point-of-transform-print g transform guess)
  (fixed-point-print (transform g) guess))

; yet another way of implementing newton's method
; with function composition and generic fixed-point for transforms
(define (newton-method-new g guess)
  (define (newton-transform g)   ; f(x) = x - g(x) / g'(x) (take intersection of tangent with y=0)
    (lambda (x)
      (- x (/ (g x) ((deriv g) x)))))
  (fixed-point-of-transform g (compose average-damp newton-transform) guess))

; Ex1.40
(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

; Ex1.41
(define (double f)
  (lambda (x) (f (f x))))
(define (inc x) (+ x 1))

; double is doubled then doubled, resulting in double being applied 4 times;
; 4 times of double on inc results in +16
(define shitty-inc
  ((double (double double)) inc))

; Ex1.42
; ((my-compose square inc) 6) gives 49
(define (my-compose f g)
  (lambda (x) (f (g x))))

; Ex1.43
; ((repeated square 2) 5) gives 625
(define (repeated f n)
  (define (id x) x)
  (if (zero? n)
      id
      (compose f (repeated f (- n 1)))))

; Ex1.44
; calculate average, variadic verion
(define (average . args)
  (define (sum xs)
    (if (empty? xs)
        0
        (+ (car xs) (sum (cdr xs)))))
  (if (empty? args)
      (error "average: empty list")
      (/ (sum args) (length args))))

; smooth a function by averaging its value in its neighbourhood
; n-times smoothing a function f:
; ((repeated smooth n) f)
(define (smooth f)
  (define dx 1e-10)
  (lambda (x) (average (f (- x dx))
                       (f x)
                       (f (+ x dx)))))

; Ex1.45
; test for least amount of times of average-damp needed for convergence
; result: [2^n, 2^(n+1)) : n times
; reason: UNKNOWN
(define (nth-root-experiment x n times)
  (define (target-f t)
    (/ x (expt t (- n 1))))
  (fixed-point-of-transform
   target-f (repeated average-damp times) 1.0))

(define (reliable-nth-root x n)
  (define times (floor (log n 2)))
  (nth-root-experiment x n times))

; Ex1.46
; iterative-improve takes unary functions good-enough? and improve as args,
; returning a function that accepts a guess and returns the desired result
(define (iterative-improve good-enough? improve)
  (lambda (x)
    (if (good-enough? x)
        x
        ((iterative-improve good-enough? improve) (improve x)))))

; sqrt using iterative-improve
(define (abstract-sqrt x)
  (define (good-enough? t)
    (< (abs (- (square t) x)) 1e-10))
  (define (improve t)
    (average t (/ x t)))     ; get closer to the fixed-point
  ((iterative-improve good-enough? improve) 1.0))
