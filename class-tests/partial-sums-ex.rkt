#lang racket
(require r5rs)
(define env (scheme-report-environment 5))
(eval '(define (stream-car stream) (car stream)) env)
(eval '(define (stream-cdr stream) (force (cdr stream))) env)
(eval '(define-syntax cons-stream
  (syntax-rules ()
    [(cons-stream x y) (cons x (delay y))])) env)

(eval '(define the-empty-stream '()) env)
(eval '(define (stream-null? stream) (null? stream)) env)
         
(eval '(define (stream-ref s n)  ;get the nth item from s. n starts from 
  (if (stream-null? s) the-empty-stream
      (if (= n 0)
          (stream-car s)
          (stream-ref (stream-cdr s) (- n 1)))))
      env)

(eval '(define (display-stream s n) ;display first n items of s
  (if (= n 0)
      (displayln "")
      (begin (display (stream-car s)) (display " ") (display-stream (stream-cdr s) (- n 1)))))
      env)


(eval '
(define (partial-sums-ex op s)
  ;; //// 
  (define (stream-op s1 s2)
    (cons-stream (op (stream-car s1) (stream-car s2))
                 (stream-op (stream-cdr s1) (stream-cdr s2))))
  (define (partial-sums s)
    (define ret
      (cons-stream (stream-car s)
                   (stream-op ret (stream-cdr s))))
    ret)
  (define (stream-combine s1 s2)
    (cons-stream (stream-car s1)
                 (cons-stream (stream-car s2)
                              (stream-combine (stream-cdr s1)
                                              (stream-cdr s2)))))
  (define (take s)
    (cons-stream (stream-car s)
                 (take (stream-cdr (stream-cdr s)))))
  (stream-combine (partial-sums (take s))
                  (partial-sums (take (stream-cdr s)))))
;// Your Code Here
env)

(define (myloop)
  (define (eval-codes codes last-val)
    (if (null? codes)
        last-val
        (eval-codes (cdr codes) (eval (car codes) env))))
    
  (let ((codes (read)))
    (if (eq? codes eof)
        (void)
        (begin (displayln (eval-codes codes (void))) (myloop)))))
(myloop)