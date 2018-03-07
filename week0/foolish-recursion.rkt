#lang racket

(define (read-input)
  (let ((r (read))
        (w (read)))
    (if (or (eq? r eof)
            (eq? w eof))
        eof
        (cons r w))))

(define (work pair)
  (define (print-line curr n)
    (if (> curr n)
        (newline)
        (begin (display curr) (display ",")
               (print-line (+ curr 1) n))))
  (define (lines-iter curr line-n n)
    (if (> curr n)
        (void)
        (begin (print-line 1 line-n)
               (lines-iter (+ curr 1) line-n n))))
  (lines-iter 1 (cdr pair) (car pair)))

(define (main)
  (let ((input (read-input)))
    (if (eq? input eof)
        (void)
        (begin (work input) (main)))))

(main)