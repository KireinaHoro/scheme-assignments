#lang racket
; Ex2.57 & Ex2.58 in separate file: infix-deriv.rkt

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) null)
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; Ex2.59
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

(define (remove-dup l)
  (define (remove s ll)
    (filter
     (lambda (x) (not (eq? s x)))
     ll))
  (cond ((null? l) l)
        (else
         (cons (car l)
               (remove-dup (remove (car l) (cdr l)))))))

(define (relative-compliment s1 s2)
  (sort (filter (lambda (s)
                  (not (element-of-set? s s2))) s1) <))
(define (symmetric-difference s1 s2)
  (sort (union-set (relative-compliment s1 s2)
                   (relative-compliment s2 s1)) <))

(define (preprocess s)
  (sort (remove-dup s) <))

(define (main)
  (let loop ((ss1 (read))
             (ss2 (read)))
    (if (eq? eof ss1)
        (void)
        (let ((s1 (preprocess ss1))
              (s2 (preprocess ss2)))
          (begin
            (display (relative-compliment s1 s2))
            (display (symmetric-difference s1 s2))
            (newline)
            (loop (read) (read)))))))
(main)