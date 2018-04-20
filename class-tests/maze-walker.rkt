#lang racket

(define N (read))
(define R #f)
(define C #f)
(define K #f)

(define (make-table r c v)
  (define t (make-hash))
  (for* ([x (in-range c)]
         [y (in-range r)])
    (table-set! t x y v))
  t)
(define (table-set! t x y v)
  (hash-set! t (list x y) v))
(define (table-ref t x y)
  (hash-ref t (list x y) #f))

(define (get-offset s)
  (case s
    ['u '(0 . -1)]
    ['d '(0 . 1)]
    ['l '(-1 . 0)]
    ['r '(1 . 0)]))
(define (transform-pos p s)
  (let ([ss (get-offset s)])
    (cons (+ (car p) (car ss))
          (+ (cdr p) (cdr ss)))))
(define (valid-pos? p)
  (let ([x (car p)]
        [y (cdr p)])
    (and (>= x 0) (>= y 0)
         (< x C) (< y R))))

(define map #f)
(define steps #f)

(define (map-ref m p)
  (table-ref m (car p) (cdr p)))
(define (map-set! m p v)
  (table-set! m (car p) (cdr p) v))
(define (read-map)
  (define (proc v)
    (cond [(eq? v 'B) 0]
          [(eq? v 'W) 100000]
          [else 1]))
  (set! R (read))
  (set! C (read))
  (set! K (read))
  (set! map (make-table R C #f))
  (set! steps (make-table R C 100000))
  (table-set! steps 0 0 0)
  (let lr ((iy 0))
    (unless (= iy R)   
      (let lc ((ix 0))
        (unless (= ix C)
          (table-set! map ix iy (proc (read)))
          (lc (+ ix 1))))
      (lr (+ iy 1)))))

(define (walk pos)
  (define (j s)
    (valid-pos? (transform-pos pos s)))
  (define (w s)
    (define (proc-m m dst)
      (- m (map-ref map dst)))
    (define (proc-s s)
      (+ s 1))
    (let* ([newp (transform-pos pos s)]
           [old-K K]
           [new-K (proc-m K newp)]
           [curr-s (map-ref steps pos)])
      (when (and
             (> (map-ref steps newp) (proc-s curr-s))
             (>= (proc-m K newp) 0))
        (map-set! steps newp (proc-s curr-s))
        (set! K new-K)
        (walk newp)
        (set! K old-K))))
  (for-each w (filter j '(u d l r))))

(define (run-eval)
  (read-map)
  (walk '(0 . 0))
  (displayln
   (let* ([pos (cons (- C 1) (- R 1))]
          [s (map-ref steps pos)])
     (if (> s 10000)
         'inf
         s))))

(for ([x (in-range N)])
  (run-eval))
