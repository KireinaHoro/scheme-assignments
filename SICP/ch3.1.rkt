#lang racket

;; Ex3.1
(define (make-accumulator s)
  (define i s)
  (lambda (x)
    (set! i (+ x i))
    i))

;; Ex3.2
(define (make-monitored f)
  (define counter 0)
  (match-lambda
    ['how-many-calls? counter]
    ['reset-count (set! counter 0)]
    [n (begin (set! counter (add1 counter))
              (f n))]))

;; Ex3.3 & Ex3.4
(define (make-account money password)
  (define m money)
  (define t 0)
  (define (deposit x)
    (set! m (+ x m))
    m)
  (define (withdraw x)
    (if (< m x)
        "Insufficient funds"
        (begin (set! m (- m x))
               m)))
  (define/match (dispatch cmd)
    (('withdraw) withdraw)
    (('deposit) deposit)
    ((_) (error "Unknown command -- DISPATCH")))
  (define (auth pass)
    (if (eq? pass password)
        (begin (set! t 0)
               dispatch)
        (if (< t 7)
            (begin (set! t (add1 t))
                   (error "Incorrect password"))
            (error (call-the-cops)))))
  (lambda (pass cmd)
    ((auth pass) cmd)))
(define (call-the-cops) (displayln "COPS CALLED"))
;; Ex3.7
(define (make-joint original original-pass password)
  (define t 0)
  (define (forward cmd)
    (original original-pass cmd))
  (define (auth pass)
    (if (eq? pass password)
        (begin (set! t 0)
               forward)
        (if (< t 7)
            (begin (set! t (add1 t))
                   (error "Incorrect password"))
            (error (call-the-cops)))))
  (lambda (pass cmd)
    ((auth pass) cmd)))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond [(= trials-remaining 0)
           (/ trials-passed trials)]
          [(experiment)
           (iter (sub1 trials-remaining) (add1 trials-passed))]
          [else
           (iter (sub1 trials-remaining) trials-passed)]))
  (iter trials 0))

;; Ex3.5
(define (estimate-integral p x1 x2 y1 y2 trials)
  (define (range-random inf sup)
    (+ inf (* (random) (- sup inf))))
  (* (* (- y2 y1)
        (- x2 x1))
     (monte-carlo trials
                  (lambda ()
                    (p (range-random x1 x2)
                       (range-random y1 y2))))))

;; Ex3.8
(define (f i)
  (define evaluated? #f)
  (if evaluated?
      0
      (begin (set! evaluated? #t)
             (if (= 0 i) 0 1))))