#lang racket

(define *table* '())
(define (intersection i1 i2)
  (let* ([l1 (car i1)]
         [r1 (cdr i1)]
         [l2 (car i2)]
         [r2 (cdr i2)]
         [maxl (max l1 l2)]
         [minr (min r1 r2)]
         [ret (- minr maxl -1)])
    (if (negative? ret)
        0
        ret)))

(define (query l r)
  (let ([q (cons l r)])
    (foldr + 0
           (map (lambda (x) (* (intersection (car x) q)
                               (cdr x)))
                *table*))))

(define (increment l r x)
  (set! *table* (cons (cons (cons l r) x)
                      *table*)))

  (let loop ([m (read)])
    (if (= m 0) (void)
        (let* ([op (read)]
               [l (read)]
               [r (read)
               [x (when (= op 1) (read))])
          (if (= op 1)
              (increment l r x)
              (displayln (query l r)))
          (loop (- m 1)))))
  