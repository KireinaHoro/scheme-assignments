#lang racket

; tagged data type
(define (attach-tag tag content)
  (cond [(eq? tag 'scheme-number) content]
        [else (cons tag content)]))
(define (type-tag datum)
  (cond [(number? datum) 'scheme-number]
        [(pair? datum) (car datum)]
        [else
         (error "Bad tagged datum -- TYPE-TAG" datum)]))
(define (contents datum)
  (cond [(number? datum) datum]
        [(pair? datum) (cdr datum)]
        [else
         (error "Bad tagged datum -- CONTENTS" datum)]))

; global function table
(define *table* (make-hash))
(define (put op type-tags f)
  (hash-set! *table* (list op type-tags) f))
(define (get op type-tags)
  (hash-ref *table* (list op type-tags) #f))

; apply operation to tagged data
(define (apply-generic op . args)
  (let* ([type-tags (map type-tag args)]  ; parameter type list
         [proc (get op type-tags)])       ; real processor
    (if proc
        (apply proc (map contents args))
        (error "No method for these types -- APPLY-GENERIC"
               (list op type-tags)))))

; handle normal scheme numbers
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number) =)
  (put '=zero? '(scheme-number) (lambda (x) (= x 0)))
  'done)
(install-scheme-number-package)
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

; handle rational numbers
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
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
  (define (equ? x y) (and (= (numer x) (numer y))
                          (= (denom x) (denom y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational) equ?)
  (put '=zero? '(rational)
       (lambda (x) (equ? x (make-rat 0 1))))
  'done)
(install-rational-package)
(define (make-rational n d)
  ((get 'make 'rational) n d))

; handle complex numbers
; need rectangular and polar packages
(define (install-complex-package)
  ;; the rectangular and polar packages
  (define (install-rectangular-package)
    ;; internal functions
    (define real-part car)
    (define imag-part cdr)
    (define (magnitude x)
      (sqrt (+ (sqr (real-part x))
               (sqr (imag-part x)))))
    (define (angle x)
      (atan (/ (imag-part x)
               (real-part x))))
    ;; exported functions
    (define (tag x) (attach-tag 'rectangular x))
    (put 'make-from-real-imag 'rectangular
         (lambda (x y) (tag (cons x y))))
    (put 'real-part '(rectangular) real-part)
    (put 'imag-part '(rectangular) imag-part)
    (put 'magnitude '(rectangular) magnitude)
    (put 'angle '(rectangular) angle)
    'done)
  (define (install-polar-package)
    ;; internal functions
    (define magnitude car)
    (define angle cdr)
    (define (real-part x)
      (* (magnitude x)
         (cos (angle x))))
    (define (imag-part x)
      (* (magnitude x)
         (sin (angle x))))
    ;; exported functions
    (define (tag x) (attach-tag 'polar x))
    (put 'make-from-mag-ang 'polar
         (lambda (x y) (tag (cons x y))))
    (put 'real-part '(polar) real-part)
    (put 'imag-part '(polar) imag-part)
    (put 'magnitude '(polar) magnitude)
    (put 'angle '(polar) angle)
    'done)
  ;; imported procedures from rectangular and polar packages
  (install-rectangular-package)
  (install-polar-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (real-part x) (apply-generic 'real-part x))
  (define (imag-part x) (apply-generic 'imag-part x))
  (define (magnitude x) (apply-generic 'magnitude x))
  (define (angle x) (apply-generic 'angle x))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (equ? x y) (and (= (real-part x) (real-part y))
                          (= (imag-part x) (imag-part y))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ? '(complex complex) equ?)
  (put '=zero? '(complex)
       (lambda (x) (equ? x (make-from-real-imag 0 0))))
  'done)
(install-complex-package)
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;(define a (make-complex-from-real-imag 1 3))
;(define b (make-complex-from-mag-ang 2 (/ pi 4)))
