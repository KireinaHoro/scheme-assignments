(define (square x) (* x x))

(define (smallest-div n)
  (find-div n 2))

(define (find-div n test-div)
  (cond ((> (square test-div) n) n)
        ((div? test-div n) test-div)
        (else (find-div n (+ test-div 1)))))

(define (div? a b)
  (= (remainder b a) 0))

(define (naive-prime? n)
  (= n (smallest-div n)))

; calculate pow(base, exp) modulo m
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (random* n)
  (if (< n 4294967087) (random n) (random 4294967087)))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random* (- n 1))))) ; pick a random number from 2 to n

(define (fast-prime? n)
  (define max-tries 10)
  (define (worker times)
    (cond ((= times 0) true)
          ((fermat-test n) (worker (- times 1))) ; try again with a new number
          (else false)))                         ; not a prime
  (if (worker max-tries)                         ; chances are that it's a Carmichael number
      (naive-prime? n)                           ; see if it's a real prime
      false))

; modified expmod used in Miller-Rabin test that returns
; '(0 . #t) on discovering a non-trivial square root of
; 1 modulo n
(define (mr-expmod base exp m)
  (cond ((= exp 0) (cons 1 false))
        ((even? exp)
         (let ((res (mr-expmod base (/ exp 2) m)))
           (if (cdr res) res
               (let ((tmp (remainder (square (car res)) m)))
                 (if (and (= tmp 1)
                          (not (= (car res) (- m 1))) ; non-trivial: != m - 1
                          (not (= (car res) 1)))      ; non-trivial: != 1
                     (cons 0 true)
                     (cons tmp false))))))
        (else
         (let ((res (mr-expmod base (- exp 1) m)))
           (if (cdr res) res
               (cons (remainder (* base (car res)) m)
                     false))))))

; Miller-Rabin test for given n against a randomly-picked a
; TODO: this is BROKEN; needs debugging
(define (mr-test n)
  (define (try-it a)
    (let ((res (mr-expmod a (- n 1) n)))
      (or (not (cdr res))
          (eq? (car res) 1))))
  (try-it (+ 1 (random* (- n 1)))))

(define (mr-prime? n)
  (define max-tries 10)  ; try 10 times
  (define (worker times)
    (cond ((= 0 times) true)
          ((mr-test n) (worker (- times 1)))
          (else false)))
  (if (= n 1) false
      (worker max-tries)))

(define (get-primes tester n count)
  (define (find-from i)
    (if (tester i) i
        (find-from (+ 1 i))))
  (define (worker curr counter)
    (cond ((>= counter count) (void))
          (else
           (let ((p (find-from curr)))
             (begin (displayln p) (worker (+ 1 p) (+ counter 1)))))))
  (worker n 0))
