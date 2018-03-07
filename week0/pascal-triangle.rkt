#lang racket

(define (pascal row col)
  (define (fact n)
    (if (<= n 1)
        1
        (* n (fact (- n 1)))))
  (/ (fact (- row 1)) (* (fact (- col 1)) (fact (- row col)))))

(define (print-pascal n)
  (define (single-line-recur nu curr)
    (if (> curr nu)
        (newline)
        (begin (display " ") (display (pascal nu curr))
               (single-line-recur nu (+ curr 1)))))
  (define (single-line nu)
    (if (= nu 1)
        (displayln 1)
        (begin (display 1) (single-line-recur nu 2))))
  (define (lines curr)
    (if (> curr n)
        (void)
        (begin (single-line curr) (lines (+ 1 curr)))))
  (lines 1))

(define (main)
  (let ((n (read)))
    (if (eq? n eof)
        (void)
        (begin (print-pascal n) (main)))))

(main)