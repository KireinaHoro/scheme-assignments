#lang racket

(define (memo-proc proc)
  (let ([already-run? #f]
        [result #f])
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? #t)
                 result)
          result))))
(define-syntax-rule (delay x)
  (memo-proc (lambda () x)))
(define (force x)
  (x))

(define the-empty-stream 'empty-stream)
(define (stream-null? s) (eq? s the-empty-stream))
(define (stream-ref s n)
  (if (zero? n)
      (stream-car s)
      (stream-ref (stream-cdr s) (sub1 n))))
(define (stream-filter pred s)
  (cond [(stream-null? s) the-empty-stream]
        [(pred (stream-car s))
         (cons-stream (stream-car s)
                      (stream-filter pred (stream-cdr s)))]
        [else (stream-filter pred (stream-cdr s))]))
(define (stream-map proc . s)
  (if (stream-null? (car s))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car s))
       (apply stream-map
              (cons proc (map stream-cdr s))))))
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))
(define (display-stream s)
  (stream-for-each displayln s))
(define (stream-take s n)
  (if (zero? n) '()
      (cons (stream-car s) (stream-take (stream-cdr s)
                                        (sub1 n)))))

(define-syntax-rule (cons-stream a b)
  (cons a (delay b)))
(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))

(define (divisible? x a)
  (zero? (remainder x a)))
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (add1 n))))
#|
(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))
(define primes (sieve (integers-starting-from 2)))
|#

(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))
(define fibs
  (cons-stream 0 (cons-stream 1
                              (add-streams (stream-cdr fibs)
                                           fibs))))

(define primes
  (cons-stream 2
               (stream-filter prime? (integers-starting-from 3))))
(define (prime? n)
  (define (iter s)
    (cond [(> (sqr (stream-car s)) n) #t]
          [(divisible? n (stream-car s)) #f]
          [else (iter (stream-cdr s))]))
  (iter primes))

;; Ex3.54
(define (mul-streams s1 s2)
  (stream-map * s1 s2))
(define factorials
  (cons-stream 1 (mul-streams (integers-starting-from (add1 (stream-car factorials)))
                              factorials)))

;; Ex3.55
(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (stream-cdr s)
                            (partial-sums s))))

;; Ex3.56
(define (merge s1 s2)
  (cond [(stream-null? s1) s2]
        [(stream-null? s2) s1]
        [else
         (let ([s1car (stream-car s1)]
               [s2car (stream-car s2)])
           (cond [(> s1car s2car) (cons-stream s2car (merge s1 (stream-cdr s2)))]
                 [(< s1car s2car) (cons-stream s1car (merge s2 (stream-cdr s1)))]
                 [else (cons-stream s1car (merge (stream-cdr s1) (stream-cdr s2)))]))]))
(define (repeat a) (cons-stream a (repeat a)))
(define (scale-stream s scale)
  (let ([ss (repeat scale)])
    (mul-streams s ss)))
(define S (cons-stream 1 (merge (merge (scale-stream S 2)
                                       (scale-stream S 3))
                                (scale-stream S 5))))

(define (sqrt-improve guess x)
  (/ (+ guess (/ x guess)) 2))
(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))
(define (euler-transform s)
  (let ([s0 (stream-ref s 0)]
        [s1 (stream-ref s 1)]
        [s2 (stream-ref s 2)])
    (cons-stream (- s2 (/ (sqr (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))
(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform (transform s))))
(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

;; this function assumes that s and t are infinite streams
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))
(define (interleave s1 s2)
  (if (stream-null? s1) s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))