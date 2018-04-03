#lang racket
(define (remove-dup l)
  (define (remove s ll)
    (filter
     (lambda (x) (not (eq? s x)))
     ll))
  (cond ((null? l) l)
        (else
         (cons (car l)
               (remove-dup (remove (car l) (cdr l)))))))

(define (repeat1 f n)
  (cond ((eq? n 0) (lambda (x) x))
        ((eq? n 1) f)
        (else
         (compose1 f (repeat1 f (- n 1))))))

(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append (map (lambda (x)
                       (cons (car s) x))
                     rest)
                rest))))

(define (preprocess s)
  (sort (remove-dup s) <))

(define (main)
  (let loop ((l (read))
             (n (read)))
    (if (eq? eof l)
        (void)
        (begin (displayln
                ((repeat1 subsets n) (preprocess l)))
               (loop (read) (read))))))
(main)
  