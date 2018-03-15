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
(define (add n1 n2)
  (lambda (f)
    (compose (n1 f) (n2 f))))

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
; Ex2.17
(define (last-pair list)
  (cond ((null? list) (error "last-pair: empty list given"))
        ((null? (cdr list)) list)
        (else (last-pair (cdr list)))))

; Ex2.18
(define (reverse lst)
  (cond ((null? lst) lst)
        ((null? (cdr lst)) lst)
        (else
         (append (reverse (cdr lst)) (list (car lst))))))

; Ex2.19
(define (cc amount coin-values)
  (define no-more? null?)
  (define except-first-denomination cdr)
  (define first-denomination car)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

; Ex2.20
; test with (same-parity 1 2 3 4 5 6 7) / (same-parity 2 3 4 5 6 7)
(define (same-parity h . l)
  (define parity (remainder h 2))
  (define (find-parity l)
    (cond ((null? l) l)
          ((eq? (remainder (car l) 2) parity)
           (cons (car l) (find-parity (cdr l))))
          (else
           (find-parity (cdr l)))))
  (cons h (find-parity l)))

; Ex2.21
(define (square x) (* x x))
(define (square-list-1 items)
  (if (null? items)
      null
      (cons (square (car items)) (square-list-1 (cdr items)))))

(define (square-list-2 items)
  (map square items))

; Ex2.23
(define (my-for-each f l)
  (if (null? l)
      (void)
      (begin (f (car l)) (my-for-each f (cdr l)))))

; Ex2.24
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (list? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

; Ex2.27
(define (deep-reverse l)
  (cond ((null? l) l)
        ((list? (car l))
         (append (deep-reverse (cdr l))
                 (list (deep-reverse (car l)))))
        (else
         (append (deep-reverse (cdr l))
                 (list (car l))))))

; Ex2.28
(define (fringe l)
  (cond ((null? l) l)
        ((list? (car l))
         (append (fringe (car l)) (fringe (cdr l))))
        (else
         (append (list (car l)) (fringe (cdr l))))))

; Ex2.29
; a) data structure functions
(define make-mobile list)
(define make-branch list)
(define left-branch car)
(define right-branch cadr)
(define branch-length left-branch)
(define branch-contents right-branch)

; d) use cons for representation
(define make-mobile-new cons)
(define make-branch-new cons)
(define left-branch-new car)
(define right-branch-new cdr)
(define branch-length-new left-branch-new)
(define branch-contents-new right-branch-new)

; b) calculate the total weight of a mobile
(define (total-weight m)
  (define (proc-contents c)
    (if (list? c)
        (total-weight c)
        c))
  (let ((left (branch-contents (left-branch m)))
        (right (branch-contents (right-branch m))))
    (+ (proc-contents left)
       (proc-contents right))))

; c) test if a mobile is balanced
(define (balanced? m)
  (define mobile? list?)
  (define (get ff choice)
    (define f
      (if (eq? choice 0)
          (compose1 ff left-branch)
          (compose1 ff right-branch)))
    (f m))
  (define (get-c choice) (get branch-contents choice)) ; get contents based on choice
  (define (get-l choice) (get branch-length choice))   ; get length based on choice
  (define (test choice) (mobile? (get-c choice)))      ; test if is mobile based on choice
  (define (sub-balanced? choice)                       ; test if branch is balanced
    (if (test choice)
        (balanced? (get-c choice))
        #t))
  (define (torque choice)              ; calculate torque based on choice
    (let ((c (get-c choice)))
      (* (get-l choice)
         (if (test choice)
             (total-weight (get-c choice))
             (get-c choice)))))
  (and (sub-balanced? 0)               ; the branches should be balanced...
       (sub-balanced? 1)
       (eq? (torque 0) (torque 1))))   ; and the mobile itself

; mobiles for test purposes
(define test-mobile-unbalanced    ; an unbalanced mobile
  (make-mobile
   (make-branch 3 5)
   (make-branch 5 (make-mobile
                   (make-branch 1 3)
                                     (make-branch 2 4)))))
(define test-mobile-balanced      ; a balanced mobile
  (make-mobile
   (make-branch 2 5)
   (make-branch 1 (make-mobile
                   (make-branch 3 (make-mobile
                                   (make-branch 1 2)
                                   (make-branch 1 2)))
                   (make-branch 2 (make-mobile
                                   (make-branch 1 4)
                                   (make-branch 2 2)))))))

; Ex2.30
; test with (square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(define (square-tree t)
  (map (lambda (sub-t)
         (cond ((not (list? sub-t)) (square sub-t))
               ((null? sub-t) sub-t)
               (else
                (square-tree sub-t))))
       t))

(define (square-tree-direct t)
  (cond ((not (list? t)) (square t))
        ((null? t) t)
        (else
         (cons (square-tree-direct (car t))
               (square-tree-direct (cdr t))))))

; Ex2.31
; test with (define (st t) (tree-map square t))
(define (tree-map f t)
  (map (lambda (sub-t)
         (cond ((not (list? sub-t)) (f sub-t))
               ((null? sub-t) sub-t)
               (else
                (tree-map f sub-t))))
       t))

; Ex2.32
; test with (subsets (list 1 2 3))
(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x)
                            (cons (car s) x))
                          rest)))))
