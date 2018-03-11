#lang racket

; utility function
(define (average . args)
  (if (empty? args)
      (error "average: empty parameter list")
      (/ (foldl + 0 args) (length args))))

; definition for rational numbers
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

; Ex2.1
(define (make-rat n d)
  (define g (gcd n d))
  (let ((new-n (abs (/ n g)))
        (new-d (abs (/ d g))))
    (cond ((xor (positive? n) (positive? d))
           (cons (- new-n) new-d))
          (else
           (cons new-n new-d)))))

(define numer car)

(define denom cdr)

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

; Ex2.2
(define (make-point x y) (cons x y))
(define (x-point t) (car t))
(define (y-point t) (cdr t))
(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment start end) (cons start end))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (midpoint s)
  (define new-term (lambda (f)
                     (average (f (start-segment s)) (f (end-segment s)))))
  (make-point (new-term x-point) (new-term y-point)))

; Ex2.3
(define (make-rect top-left bottom-right)
  (cons top-left bottom-right))
(define (length-rect r)
  (abs (- (x-point (car r)) (x-point (cdr r)))))
(define (width-rect r)
  (abs (- (y-point (car r)) (y-point (cdr r)))))

(define (area-rect r)
  (* (width-rect r) (length-rect r)))
(define (perimeter-rect r)
  (* 2 (+ (width-rect r) (length-rect r))))

; pair implemented with procedures
(define (my-cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "dispatch: argument not 0 or 1 from cons" m))))
  dispatch)
(define (my-car z) (z 0))
(define (my-cdr z) (z 1))

; Ex2.4
(define (other-cons x y)
  (lambda (m) (m x y)))
(define (other-car z)
  (z (lambda (p q) p)))
(define (other-cdr z)
  (z (lambda (p q) q)))

; Ex2.5
(define (max-expts x d)
  (define (worker curr)
    (if (or (= 1 curr) (not (zero? (remainder curr d))))
        0
        (+ 1 (worker (/ curr d)))))
  (worker x))
(define (num-cons x y)
  (* (expt 2 x) (expt 3 y)))
(define (num-car z)
  (max-expts z 2))
(define (num-cdr z)
  (max-expts z 3))

; Ex2.6
; Church numerals: denote a (nonnegative) number with the times of given function
; applied in a transform

; for demonstrating the process; call with ((n base) 0) to see corresponding number
(define (base x) (+ 1 x))

(define zero (lambda (f) (lambda (x) x)))   ; \_ -> ids
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))  ; apply f once more time
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

; Ex2.7 - interval arithmetics
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

; Ex2.10
(define (div-interval x y)
  (if (zero? (- (upper-bound y) (lower-bound y)))
      (error "div-interval: divide by zero-span interval" y)
      (mul-interval x 
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define (make-interval a b) (cons a b))
(define (upper-bound z) (cdr z))
(define (lower-bound z) (car z))

; Ex2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

; Ex2.12
(define (make-center-percent c p)
  (make-center-width c (* c p)))
(define (percent i)
  (/ (width i) (center i)))

; Ex2.13
(define (mul-interval-percent x y)
  (make-center-percent (* (center x)                            ; c1 * c2 * (p1 * p2 + 1)
                          (center y)
                          (+ 1 (* (percent x) (percent y))))
                       (/ (+ (percent x) (percent y))           ; (p1 + p2) / (p1 * p2 + 1)
                          (+ 1 (* (percent x) (percent y))))))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))