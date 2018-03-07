#lang racket

(define (max list)
  (define (max-worker curr list)
    (cond ((null? list) curr)
          ((< curr (car list))
           (max-worker (car list) (cdr list)))
          (else
           (max-worker curr (cdr list)))))
  (max-worker -1000 list))

(define (read-input)
  (define (read-input-worker ret times)
    (if (= 0 times)
        ret
        (let ((a (read)))
          (read-input-worker
           (cons a ret)
           (- times 1)))))
  (let ((n (read)))
    (read-input-worker '() n)))

(define (main)
  (define (main-loop n)
    (if (= 0 n)
        (void)
        (begin (displayln (max (read-input)))
               (main-loop (- n 1)))))
  (let ((n (read)))
    (main-loop n)))

(main)