#lang racket

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq) (accumulate op init (cdr seq)))))

(define (enumerate-interval a b)
  (if (> a b)
      '()
      (cons a (enumerate-interval (+ a 1) b))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))


(define (tri-num-list n s)
  ;
  (define (triple-wanted? t)
    (eq? s (+ (car t) (cadr t) (caddr t))))
  (sort
   (filter triple-wanted?
                   (flatmap (lambda (x)
                              (map (lambda (p) (append p (list x)))
                                   (unique-pairs (- x 1))))
                            (enumerate-interval 1 n)))
   (lambda (x y)
     (cond ((< (car x) (car y)) true)
           ((and (= (car x) (car y)) (< (cadr x) (cadr y))) true)
           ((and (= (car x) (car y)) (= (cadr x) (cadr y))) (< (caddr x) (caddr y)) true)
           (else false)))))
(define (unique-pairs n)
  (define (remove x l)
    (filter (lambda (xx) (not (eq? x xx)))
            l))
  (flatmap (lambda (x)
             (map (lambda (t) (list t x))
                  (enumerate-interval 1 (- x 1))))
           (enumerate-interval 1 n)))
;
(define (myloop)
  (let ((n (read))
        (s (read)))
    (if (eq? n eof)
        (void)
        (begin (display (tri-num-list n s)) (newline) (myloop)))))

(myloop)