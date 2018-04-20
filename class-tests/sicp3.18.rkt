#lang racket
(require r5rs)
(define env (scheme-report-environment 5))

(eval '(define (last-pair lst)
         (if (null? (cdr lst))
             lst
             (last-pair (cdr lst))))
      env)

(eval '(define (make-cycle lst)
         (set-cdr! (last-pair lst) lst)
         lst)
      env)

(eval
 '
 ;;
 (define (check-cycle l)
   (define (hash-set! t k v)
     (cond [(eq? (car t) '()) (set-car! t (cons k v))]
           [(eq? (cdr t) '()) (set-cdr! t (cons (cons k v) '()))]
           [(eq? (caar t) k) (set-cdr! (car t) v)]
           [else (hash-set! (cdr t) k v)]))
   (define (hash-ref t k vv)
     (cond [(eq? (car t) '()) vv]
           [(eq? (caar t) k) (cdar t)]
           [(eq? (cdr t) '()) vv]
           [else (hash-ref (cdr t) k vv)]))
   (define *table* '(()))
   (define (set-visited! x)
     (hash-set! *table* x #t))
   (define (visited? x)
     (hash-ref *table* x #f))
   (cond [(null? l) #f]
         [(visited? l) #t]
         [else
          (begin (set-visited! l)
                 (check-cycle (cdr l)))]))
 ;;
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