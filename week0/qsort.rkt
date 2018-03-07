#lang racket

(define (read-input)
  (define (read-input-iter ret)
    (let ((a (read)))
      (if (eq? a eof)
          ret
          (read-input-iter (cons a ret)))))
  (read-input-iter '()))

(define (print-list list)
  (define (print-recur list)
    (if (null? list)
        (void)
        (begin (display " ") (display (car list)) (print-recur (cdr list)))))
  (begin (display (car list)) (print-recur (cdr list))))

; filter out the adjacent duplicates
(define (filter-dup l)
  (define (worker ret curr)
    (cond ((null? curr) ret)
          ((null? ret)
           (worker (append ret (list (car curr))) (cdr curr)))
          ((eq? (last ret) (car curr))
           (worker ret (cdr curr)))
          (else
           (worker (append ret (list (car curr))) (cdr curr)))))
  (worker '() l))

; sort the list
(define (qsort e)
  (if (or (null? e) (<= (length e) 1))
      e
      (let loop ((left null) (right null) (pivot (car e)) (rest (cdr e)))
        (if (null? rest)
            (append (append (qsort left) (list pivot)) (qsort right))
            (let ((operand (car rest)))
              (if (< operand pivot)
                  (loop (append left (list operand)) right pivot (cdr rest))
                  (loop left (append right (list operand)) pivot (cdr rest))))))))

; sort the list, discarding duplicates in the process
(define (qsort-w/o-dup e)
  (if (or (null? e) (<= (length e) 1))
      e
      (let loop ((left null) (right null) (pivot (car e)) (rest (cdr e)))
        (if (null? rest)
            (append (append (qsort-w/o-dup left) (list pivot)) (qsort-w/o-dup right))
            (let ((operand (car rest)))
              (cond
                ((< operand pivot)
                 (loop (append left (list operand)) right pivot (cdr rest)))
                ((> operand pivot)
                 (loop left (append right (list operand)) pivot (cdr rest)))
                (else
                 (loop left right pivot (cdr rest)))))))))


(define (main)
  (let ((input (read-input)))
    (print-list
     ;(filter-dup (qsort input))
     (qsort-w/o-dup input)
     )))

(main)