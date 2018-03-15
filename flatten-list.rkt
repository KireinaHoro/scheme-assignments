#lang racket

(define (flatten l)
  (cond ((null? l) (list))
        ((list? (car l))
         (append (flatten (car l)) (flatten (cdr l))))
        (else
         (append (list (car l)) (flatten (cdr l))))))

(define (main)
  (let ((input (read)))
    (if (eq? input eof)
        (void)
        (begin (displayln (flatten input)) (main)))))

(main)