#lang racket

; utility function
(define (average . args)
  (if (null? args)
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
;(define (base x) (+ 1 x))

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
(define make-branch-mobile list)
(define left-branch-mobile car)
(define right-branch-mobile cadr)
(define branch-length left-branch-mobile)
(define branch-contents right-branch-mobile)

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
  (let ((left (branch-contents (left-branch-mobile m)))
        (right (branch-contents (right-branch-mobile m))))
    (+ (proc-contents left)
       (proc-contents right))))

; c) test if a mobile is balanced
(define (balanced? m)
  (define mobile? list?)
  (define (get ff choice)
    (define f
      (if (eq? choice 0)
          (compose1 ff left-branch-mobile)
          (compose1 ff right-branch-mobile)))
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
   (make-branch-mobile 3 5)
   (make-branch-mobile 5 (make-mobile
                          (make-branch-mobile 1 3)
                          (make-branch-mobile 2 4)))))
(define test-mobile-balanced      ; a balanced mobile
  (make-mobile
   (make-branch-mobile 2 5)
   (make-branch-mobile 1 (make-mobile
                          (make-branch-mobile 3 (make-mobile
                                                 (make-branch-mobile 1 2)
                                                 (make-branch-mobile 1 2)))
                          (make-branch-mobile 2 (make-mobile
                                                 (make-branch-mobile 1 4)
                                                 (make-branch-mobile 2 2)))))))

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

; Ex2.33
(define (my-map p seq)
  (foldr (lambda (x y) (cons (p x) y)) null seq))
(define (my-append seq1 seq2)
  (foldr cons seq2 seq1))
(define (my-length seq)
  (foldr (lambda (_ y) (+ 1 y)) 0 seq))

; Ex2.34
; test with (eq? 79 (horner-eval 2 (list 1 3 0 5 0 1))
(define (horner-eval x coefficient-sequence)
  (foldr (lambda (this-coeff higher-terms) (+ (* higher-terms x)
                                              this-coeff))
         0
         coefficient-sequence))

; Ex2.35
; test with (count-leaves-foldr '((1 2) 3 4))
(define (count-leaves-foldr t)
  (foldr + 0 (map (lambda (t)
                    (if (list? t)
                        (count-leaves-foldr t)
                        1))
                  t)))

; Ex2.36
; test with (accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (foldr op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; Ex2.37
; matrix arithmetics
(define (dot-product v w)
  (foldr + 0 (map * w v)))
(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))
(define (transpose m)
  (accumulate-n cons null m))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (l)
           (map (lambda (c) (dot-product c l))
                cols)) m)))

; Ex2.38
; note that this is different from foldl in #lang racket:
; the shipped foldl assumes that the operator takes acc as the last argument,
; while this version assumes that acc is the first argument of op.
(define (my-foldl op initial seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial seq))

; Ex2.39
(define (reverse-a1 seq)
  (foldl cons null seq))
(define (reverse-a2 seq)
  (my-foldl (lambda (acc x) (append (list x) acc)) null seq))
(define (reverse-b seq)
  (foldr (lambda (x acc) (append acc (list x))) null seq))

(define (enumerate-interval start end)
  (define (iter curr result)
    (if (< curr start)
        result
        (iter (- curr 1)
              (cons curr result))))
  (iter end null))

; flatten lists generated by mapping with f that takes elements and returns lists
(define (flatmap proc seq)
  (foldr append null (map proc seq)))

; import fast-prime? from primality.rkt
(require (only-in "primality.rkt"
                  (fast-prime? prime?)))
(define (prime-sum? p)
  (prime? (+ (car p) (cadr p))))
(define (make-pair-sum p)
  (list (car p) (cadr p) (+ (car p) (cadr p))))

; test with (prime-sum-pairs 6)
; the result should be '((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

; calculate permutations of set by:
; for each x \in S, cons x to every permutation of S - x
(define (permutations l)
  (define (remove x l)
    (filter (lambda (xx) (not (eq? x xx)))
            l))
  (if (null? l)
      (list null)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p)) (permutations (remove x l))))
               l)))

; Ex2.40
(define (unique-pairs n)
  (define (remove x l)
    (filter (lambda (xx) (not (eq? x xx)))
            l))
  (flatmap (lambda (x)
             (map (lambda (t) (list t x))
                  (enumerate-interval 1 (- x 1))))
           (enumerate-interval 1 n)))
(define (new-prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

; Ex2.41
(define (find-magic-triple n s)
  (define (triple-wanted? t)
    (eq? s (+ (car t) (cadr t) (caddr t))))
  (filter triple-wanted?
          (flatmap (lambda (x)
                     (map (lambda (p) (append p (list x)))
                          (unique-pairs (- x 1))))
                   (enumerate-interval 1 n))))

; Ex2.42
(define (queens board-size)
  (define empty-board null)
  (define (safe? k pos)
    (define (check-cross?)
      (andmap (lambda (i)
                (not (eq? (abs (- (list-ref pos i)
                                  (car pos)))
                          i)))
              (enumerate-interval 1 (- k 1))))
    (and (null? (filter (lambda (x) (eq? x (car pos)))
                        (cdr pos)))
         (check-cross?)))
  (define (adjoin-position new-row k rest-of-queens)
    (cons new-row rest-of-queens))
  (define (queen-cols k)
    (if (eq? k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

; Ex2.44
(require (only-in sicp-pict
                  below
                  beside
                  flip-vert flip-horiz
                  segments->painter
                  transform-painter))
(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))
(define (right-split painter n)
  (if (eq? n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))
(define (up-split painter n)
  (if (eq? n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))
(define (new-flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))
(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  (compose1 flip-horiz flip-vert) flip-vert)))
    (combine4 (corner-split ;(flip-horiz ; Ex2.52.c
               painter
               ;)          ; Ex2.52.c
               n))))

; Ex2.45
; (define right-split (split beside below))
; (define up-split (split below beside))
(define (split first second)
  (lambda (painter)
    (first painter
           (second painter painter))))

; Ex2.46
(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)
(define (scale-vect s v)
  (make-vect (* (xcor-vect v) s)
             (* (ycor-vect v) s)))
(define (neg-vect v)
  (scale-vect -1 v))
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (add-vect v1 (neg-vect v2)))

; for (list 'frame 1 2 3)
(define origin-frame cadr)
(define edge1-frame caddr)
(define edge2-frame cadddr)

; Ex2.47
; 1: origin: car; edge1: cadr; edge2: caddr
; 2: origin: car; edge1: caadr; edge2: cdadr
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

; Ex2.48
;(define make-segment cons)
;(define start-segment car)
;(define end-segment cdr)

; Ex2.49
; sicp-pict module does not conform to SICP :(
; (paint p) - p: painter?
(define (outline-of-frame frame)
  (define p (segments->painter
             (list (make-segment (make-vect 0 0) (make-vect 0 1))
                   (make-segment (make-vect 0 0) (make-vect 1 0))
                   (make-segment (make-vect 1 0) (make-vect 1 1))
                   (make-segment (make-vect 0 1) (make-vect 1 1)))))
  ((apply transform-painter (list (origin-frame frame)                                  ; origin
                                  (add-vect (edge1-frame frame) (origin-frame frame))   ; bottom-right
                                  (add-vect (edge2-frame frame) (origin-frame frame)))) ; top-left
   p))
(define cross-painter
  (segments->painter (list (make-segment (make-vect 0 0) (make-vect 1 1))
                           (make-segment (make-vect 0 1) (make-vect 1 0)))))
(define diamond-painter
  (segments->painter (list (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
                           (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
                           (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
                           (make-segment (make-vect 0 0.5) (make-vect 0.5 0)))))

; Ex2.53
; (list 'a 'b 'c): '(a b c)
; (list (list 'george)): '((george))
; (cdr '((x1 x2) (y1 y2))): '((y1 y2))
; (cadr '((x1 x2) (y1 y2))): '(y1 y2)
; (pair? (car '(a short list))): #f
; (memq 'red '((red shoes) (blue socks))): #f
; (memq 'red '(red shoes blue socks)): '(red shoes blue socks)

; Ex2.54
(define (my-equal? l1 l2)
  (cond ((not (or (pair? l1) (pair? l2)))       ; both not pairs
         (eq? l1 l2))
        ((not (and (pair? l1) (pair? l2))) #f)  ; only one of l1 and l2 is pair
        (else
         (and (my-equal? (car l1) (car l2))
              (my-equal? (cdr l1) (cdr l2))))))

; Ex2.55
; (note: in Racket (car ''aa) prints 'quote)
; ''abraacadabra == (quote (quote abracadabra))
;                == (list 'quote 'abracadabra)

(define (variable? e)
  (cond ((pair? e) false)
        ((not (symbol? e)) false)
        ((eq? e '+) false)
        ((eq? e '*) false)
        ((eq? e '**) false)
        (else true)))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (sum? e)
  (and (pair? e) (eq? (car e) '+)))
(define addend cadr)
(define (augend e)
  (cond ((null? (cdddr e)) (caddr e))
        (else (cons '+ (cddr e)))))     ; Ex2.57
(define (=number? e n)
  (and (number? e) (= e n)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        (else (list '+ a1 a2))))
(define (product? e)
  (and (pair? e) (eq? (car e) '*)))
(define multiplier cadr)
(define (multiplicand e)
  (cond ((null? (cdddr e)) (caddr e))
        (else (cons '* (cddr e)))))     ; Ex2.57
(define (make-product m1 m2)
  (cond ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((or (=number? m1 0) (=number? m2 0)) 0)
        (else (list '* m1 m2))))
; Ex2.56
(define (exponentation? e)
  (and (pair? e) (eq? (car e) '**)
       (number? (exponent e))))
(define (make-exponentation b p)
  (cond ((=number? p 0) 1)
        ((=number? p 1) b)
        (else (list '** b p))))
(define base cadr)
(define exponent caddr)

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentation? exp)
         (make-product
          (deriv (base exp) var)
          (make-product (exponent exp)
                        (make-exponentation (base exp) (- (exponent exp) 1)))))
        (else
         (error "unknown expression type -- DERIV" exp))))

; Ex2.57 & Ex2.58 in separate file: infix-deriv.rkt

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) null)
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; Ex2.59
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

; Ex2.60 - multisets
(define element-of-mset? element-of-set?)
(define adjoin-mset cons)
; if a is present in both s1 and s2, take a and remove a from both s1 and s2
(define (intersection-mset s1 s2)
  (cond ((or (null? s1) (null? s2)) null)
        ((element-of-mset? (car s1) s2)
         (cons (car s1)
               (intersection-mset (cdr s1) (remove (car s1) s2))))
        (else
         (intersection-mset (cdr s1) s2))))
; take a and remove a from both s1 and s2
(define (union-mset s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else
         (cons (car s1)
               (union-mset (remove (car s1) s2)
                           (cdr s1))))))

; ascending set - test with the following
; (define s1 '(1 2 4 7 8))
; (define s2 '(2 3 4 8 9))
(define (element-of-ascending-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-ascending-set? x (cdr set)))))
(define (intersection-ascending-set s1 s2)
  (if (or (null? s1) (null? s2)) null
      (let ((x1 (car s1)) (x2 (car s2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-ascending-set (cdr s1)
                                                 (cdr s2))))
              ((< x1 x2)
               (intersection-ascending-set (cdr s1) s2))
              ((> x1 x2)
               (intersection-ascending-set s1 (cdr s2)))))))
; Ex2.61
(define (adjoin-ascending-set x s)
  (cond ((null? s) (list x))
        ((< x (car s)) (cons x s))
        (else
         (cons (car s) (adjoin-ascending-set x (cdr s))))))
; Ex2.62
(define (union-ascending-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else
         (let ((x1 (car s1)) (x2 (car s2)))
           (cond ((= x1 x2)
                  (cons x1 (union-ascending-set (cdr s1) (cdr s2))))
                 ((< x1 x2)
                  (cons x1 (union-ascending-set (cdr s1) s2)))
                 (else
                  (cons x2 (union-ascending-set s1 (cdr s2)))))))))

; binary-tree-represented sets
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry l r)
  (list entry l r))
(define (element-of-tree-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-tree-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-tree-set? x (right-branch set)))))
(define (adjoin-tree-set x set)
  (cond ((null? set) (make-tree x null null))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-tree-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-tree-set x (right-branch set))))))

; Ex2.63
; They produce the same result
; version 1: T(n) = 2*T(n / 2) + O(n / 2) => O(n * log n)
; version 2: T(n) = 2*T(n / 2) + O(1)     => O(n)
(define (tree->list-1 tree)
  (if (null? tree)
      null
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result)
    (if (null? tree)
        result
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result)))))
  (copy-to-list tree null))

; Ex2.64
; for elements that's an ordered list
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons null elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))
; Provided that the input sequence is sorted, the algorithm
; takes the first n elements requested to be constructed into
; a balanced binary tree, selects the two halves, constructs
; balanced trees recursively, and putting them together to form
; the requested balanced tree.
; T(n) = 2*T(n/2) + O(1) => O(n)

; Ex2.65
; test with:
; (define a (list->tree '(1 2 5 6 7 9)))
; (define b (list->tree '(2 3 4 8 9)))
; (union-tree-set a b)
; (tree->list (union-tree-set a b))
; (tree->list (intersection-tree-set a b))
(define tree->list tree->list-2)
(define (union-tree-set s1 s2)
  (let ((l1 (tree->list s1))
        (l2 (tree->list s2)))
    (list->tree (union-ascending-set l1 l2))))
(define (intersection-tree-set s1 s2)
  (let ((l1 (tree->list s1))
        (l2 (tree->list s2)))
    (list->tree (intersection-ascending-set l1 l2))))

; Ex2.66
(define make-record cons)
(define key car)
(define value cdr)
(define (lookup k db)
  (cond ((null? db) false)
        ((= k (key (entry db))) (value (entry db)))
        ((< k (key (entry db)))
         (lookup k (left-branch db)))
        ((> k (key (entry db)))
         (lookup k (right-branch db)))))

; Ex2.67
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define symbol-leaf cadr)
(define weight-leaf caddr)
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define left-branch-huffman car)
(define right-branch-huffman cadr)
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        null
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))     ; get back to top of tree
              (decode-1 (cdr bits) next-branch))))) ; go down deeper
  (define (choose-branch bit branch)
    (cond ((= bit 0) (left-branch-huffman branch))
          ((= bit 1) (right-branch-huffman branch))
          (else (error "bad bit -- CHOOSE-BRANCH" bit))))
  (decode-1 bits tree))
(define (adjoin-set-huffman x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set-huffman x (cdr set))))))
(define (make-leaf-set pairs)
  (if (null? pairs)
      null
      (let ((pair (car pairs)))
        (adjoin-set-huffman (make-leaf (car pair)     ; symbol
                                       (cadr pair))   ; frequency
                            (make-leaf-set (cdr pairs))))))

; Ex2.67
; sample Huffman symbol frequency tree
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
; test with (decode sample-message sample-tree)
; result: '(A D A B B C A)

; Ex2.68
; test with (equal? (encode '(A D A B B C A) sample-tree) sample-message)
(define (encode message tree)
  (if (null? message)
      null
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
(define (encode-symbol symbol tree)
  (define (in-branch? branch)
    (not (equal? false (member symbol (symbols branch)))))
  (cond ((leaf? tree) null)
        ((not (in-branch? tree))
         (error "symbol not in tree -- ENCODE-SYMBOL" symbol))
        ((in-branch? (left-branch-huffman tree))
         (cons 0 (encode-symbol symbol
                                (left-branch-huffman tree))))
        ((in-branch? (right-branch-huffman tree))
         (cons 1 (encode-symbol symbol
                                (right-branch-huffman tree))))))

; Ex2.69
; test with (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define (successive-merge s)
  (define (worker curr-set)
    (cond ((null? (cdr curr-set)) curr-set)
          (else (let ((left (car curr-set))
                      (right (cadr curr-set)))
                  (worker (cons (make-code-tree right left)
                                (cddr curr-set)))))))
  (car (worker s)))

; Ex2.70
(define lyrics-tree (generate-huffman-tree
                     #ci'((A 2) (NA 16) (BOOM 1) (SHA 3)
                                (GET 2) (YIP 9) (JOB 2) (WAH 1))))
(define lyrics #ci'(
                    Get a job 

                        Sha na na na na na na na na

                        Get a job

                        Sha na na na na na na na na

                        Wah yip yip yip yip yip yip yip yip yip

                        Sha boom
                        ))
(define (print-code code)
  (if (null? code)
      (newline)
      (begin (print (car code)) (print-code (cdr code)))))
; (print-code (encode lyrics lyrics-tree)):
; 111101110111110110000000001111011101111101100000000011111111010101010101010101101111110
; 87 bits - 36*log_2(8)=108 bits for fixed-length code

; tagged data
(define attach-tag cons)
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

; get & put
(define *table* (make-hash))
(define (put op tag-type f)
  (hash-set! *table* (list op tag-type) f))
(define (get op tag-type)
  (hash-ref *table* (list op tag-type) #f))
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types -- APPLY-GENERIC"
                 (list op type-tags))))))
; Ex2.73
(define (deriv-new exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))
(define operator car)
(define operands cdr)
; a: unless the expression is a variable or number, dispatch the expression
;    according to the tag (type) of the expression;
;    numbers and variables are not lists, thus can't have types
(define (install-sum-package)
  ;; internal functions
  (define addend car)
  (define (augend s)
    (if (null? (cddr s)) (cadr s)
        (cons '+ (cdr s))))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (deriv-sum exp var)
    (make-sum (deriv-new (addend exp) var)
              (deriv-new (augend exp) var)))
  ;; exported functions
  (put 'make '+ make-sum)
  (put 'deriv '+ deriv-sum)
  'done)
(install-sum-package)
(define (install-product-package)
  ;; internal functions
  (define multiplier car)
  (define (multiplicand p)
    (if (null? (cddr p)) (cadr p)
        (cons '* (cdr p))))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  (define (deriv-product exp var)
    ((get 'make '+)
     (make-product (multiplier exp)
                   (deriv-new (multiplicand exp) var))
     (make-product (deriv-new (multiplier exp) var)
                   (multiplicand exp))))
  ;; exported functions
  (put 'make '* make-product)
  (put 'deriv '* deriv-product)
  'done)
(install-product-package)
(define (install-exponent-package)
  ;; internal functions
  (define (base s) (car s))
  (define (exponent s) (cadr s))
  (define (make-exponentation b e)
    (cond ((=number? e 0) 1)
          ((=number? e 1) b)
          ((and (number? b) (number? e)) (expt b e))
          (else (list '** b e))))
  (define (deriv-exponentation exp var)
    (let ((make-product (get 'make '*)))
      (make-product
       (make-product
        (exponent exp)
        (make-exponentation (base exp) (- (exponent exp) 1)))
       (deriv-new (base exp) var))))
  ;; exported functions
  (put 'make '** make-exponentation)
  (put 'deriv '** deriv-exponentation)
  'done)
(install-exponent-package)
; d: with inverted position of operator and operation name, relevant put
;    clauses need modification to suit the new order

; Ex2.74
(define (install-record-package)
  ;; internal functions
  ;; exported functions
  'done)
; a: the personnel file should be a pair consisting of the
;    division identifier and the data
(define (get-record employee file)
  (let ((file-type (type-tag file)))
    (let ((getter (get 'query file-type)))
      (getter employee (contents file)))))
; b: the record should be a pair consisting of the record type
;    identifier and the data, with a salary getter available in the function table
(define (get-salary employee)
  (let ((record-type (type-tag employee)))
    (let ((getter (get 'salary record-type)))
      (getter (contents employee)))))
; c
(define (find-employee-record name file-list)
  (if (null? file-list)
      false
      (let ((find-result (get-record name (car file-list))))
        (if find-result
            find-result
            (find-employee-record name (cdr file-list))))))
; d: a new package for the new company, implementing 'query 'salary

; Ex2.75
(define (make-from-mag-ang mag ang)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) mag)
          ((eq? op 'angle) ang)
          ((eq? op 'real-part)
           (* mag (cos ang)))
          ((eq? op 'imag-part)
           (* mag (sin ang)))
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

; Ex2.76
; explicit dispatch is good for adding new operations
; data-directed style is good for both situations
; message-passing style is best for new types
