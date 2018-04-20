#lang racket

(require r5rs)

(define env (scheme-report-environment 5))
(eval
 '
 ;;
 (define (count-pairs x)
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
   (cond [(not (pair? x)) 0]
         [(visited? x) 0]
         [else          
          (set-visited! x)
          (cond
            [(equal? (car x) (cdr x))
             (+ (count-pairs (car x))
                1)]
            [else
             (+ (count-pairs (car x))
                (count-pairs (cdr x))
                1)])]))
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
