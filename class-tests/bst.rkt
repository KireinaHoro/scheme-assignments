#lang racket

(define (index-of lst ele)
  (let loop ((lst lst)
             (idx 0))
    (cond ((empty? lst) #f)
          ((equal? (first lst) ele) idx)
          (else (loop (rest lst) (add1 idx))))))

(define (judge-false-list lst)
  (call/cc
   (lambda (cc)
     (let* ([head (car lst)]
	    [tail (cdr lst)]
	    [tail-with-index
	     (map (lambda (x) (cons (index-of tail x) x)) tail)]
	    [smaller
	     (filter (lambda (x) (< (cdr x)
				    head)) tail-with-index)]
	    [larger
	     (filter (lambda (x) (> (cdr x)
				    head)) tail-with-index)])
       (for-each
	(lambda (x)
	  (for-each
	   (lambda (y)
	     (if (> (car x)
		    (car y))
		 (cc false)
		 (void))) larger)) smaller)
       true))))

(define (worker lst)
  (if (null? lst)
      true
      (and (judge-false-list lst)
	   (worker (cdr lst)))))

(define n (read))
(define (reader n)
  (if (= n 0)
      '()
      (append (reader (- n 1)) (list (read)))))
(define (main)
  (if (= n 0)
      (void)
      (begin
	(let* ([count (read)]
	       [lst (reader count)])
	  (displayln (if (worker lst)
			 "YES"
			 "NO")))
	(set! n (- n 1))
	(main))))
(main)
