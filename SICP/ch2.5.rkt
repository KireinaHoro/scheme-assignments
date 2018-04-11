#lang racket

(define (compose1-n f n)
  (if (= n 0)
      identity
      (compose1 f (compose1-n f (- n 1)))))
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

; global coercion table
(define *coercion-table* (make-hash))
(define (put-coercion src dst f)
  (hash-set! *coercion-table* (cons src dst) f))
(define (get-coercion src dst)
  (hash-ref *coercion-table* (cons src dst) #f))

; global type tower
(define *type-tower*
  (list 'scheme-number 'rational 'real 'complex))
(define (is-higher-than? a b)
  (if (eq? a b) false
      (not (eq? false (memq a (memq b *type-tower*))))))
(define (distance-in-tower a b)
  (cond [(is-higher-than? a b)
         (error "Trying to cast from higher type to lower type -- DISTANCE-IN-TOWER"
                (list a b))]
        [(eq? a b) 0]
        [else
         (+ 1 (distance-in-tower (cadr (memq a *type-tower*))
                                 b))]))


; apply operation to tagged data
(define (apply-generic op . args)
  (let* ([type-tags (map type-tag args)]  ; parameter type list
         [proc (get op type-tags)])       ; real processor
    (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
            (let* ([type1 (car type-tags)]
                   [type2 (cadr type-tags)]
                   [a1 (car args)]
                   [a2 (cadr args)]
                   [t1->t2 (get-coercion type1 type2)]
                   [t2->t1 (get-coercion type2 type1)])
              (cond [(eq? type1 type2)    ; there's no method for these two types, as we've tried before
                     (error "No method for these types -- APPLY-GENERIC"
                            (list op type-tags))]
                    [t1->t2
                     (apply-generic op (t1->t2 a1) a2)]
                    [t2->t1
                     (apply-generic op a1 (t2->t1 a2))]
                    [else              
                     (error "No method for these types -- APPLY-GENERIC"
                            (list op type-tags))]))
            (error "No method for these types -- APPLY-GENERIC"
                   (list op type-tags))))))

; Ex2.82
; limitation: exp: '(complex scheme-number)
; will try to convert to '(complex complex) and '(scheme-number scheme-number)
(define (apply-generic-type-list op . args)
  (define (get-conversion a b)
    (if (eq? a b) identity    ; don't convert if we're of the same type
        (get-coercion a b)))
  (let* ([type-tags (map type-tag args)]
         [proc (get op type-tags)])
    (if proc
        (apply proc (map contents args))
        (let loop ([types-available type-tags])
          (let* ([current-type (car types-available)]
                 [destination-types (map (const current-type) type-tags)]
                 [coercion-function-list
                  (map get-conversion type-tags destination-types)]
                 [coercion-available
                  (eq? false (memq false coercion-function-list))]
                 [proc (get op destination-types)])
            (cond [(and proc coercion-available)
                   (apply proc
                          (map (compose contents apply)
                               coercion-function-list
                               (map list args)))]
                  [(not (null? (cdr types-available)))
                   (loop (cdr types-available))]
                  [else
                   (error "No method for these types -- APPLY-GENERIC-TYPE-LIST"
                          (list op type-tags))]))))))

; Ex2.84
; raise all arguments to the type of the most derived type
(define (apply-generic-raise op . args)
  (define (raise x) ((get 'raise (list (type-tag x)))
                     x))
  (define type-tags (map type-tag args))
  (define highest-type (car (sort
                             type-tags
                             is-higher-than?)))
  (let ([proc (get op type-tags)])
    (if proc
        (apply proc (map contents args))
        (let ([proc (get op (map (const highest-type) type-tags))])
          (if proc
              (apply proc
                     (map (lambda (x)
                            (contents
                             ((compose1-n raise
                                          (distance-in-tower (type-tag x)
                                                             highest-type))
                              x)))
                          args))
              (error "No method for these types -- APPLY-GENERIC-RAISE"
                     (list op type-tags)))))))

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
  (put 'raise '(scheme-number) ; to rational
       (lambda (x) ((get 'make 'rational) x 1)))
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
  (put 'raise '(rational)   ; to real
       (lambda (x)
	 ((get 'make 'real) (/ (numer x)
			       (denom x)))))
  (put 'project '(rational) ; to scheme-number
       (lambda (x)
	 ((get 'make 'scheme-number)
	  (quotient (numer (contents x)) (denom (contents x))))))
  'done)
(install-rational-package)
(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'real
       (lambda (x) (tag x)))
  (put 'equ? '(real real) =)
  (put '=zero? '(real) (lambda (x) (= x 0)))
  (put 'raise '(real)   ; to complex
       (lambda (x)
         ((get 'make-from-real-imag 'complex)
	  (contents x) 0)))
  (put 'project '(real) ; to rational
       (lambda (x)
	 ((get 'make 'rational)
	  (* (contents x) 100000000000)
	  100000000000)))
  'done)
(install-real-package)
(define (make-real n)
  ((get 'make 'real) n))

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
  (define (scheme-number->complex n)
    (make-complex-from-real-imag (contents n) 0))
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
  (put-coercion 'scheme-number 'complex scheme-number->complex)
  (put 'project '(complex) ; to real
       (lambda (x)
	 ((get 'make 'real) (real-part x))))
  'done)
(install-complex-package)
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (drop x)
  (let ([project (get 'project (list (type-tag x)))])
    (if project
	(let* ([project-result (project x)]
	       [raise-result (apply-generic 'raise project-result)])
	  (if (equal? raise-result x)
	      (drop project-result)
	      x))
	x)))

(define a (make-complex-from-real-imag 1 3))
(define b (make-complex-from-real-imag 4 0))
(define c (make-complex-from-real-imag 4.2 0))
