#lang racket
;f(n) = f(n-1) + 4 * f(n-2) + 5 * f(n-3) - 2 * f(n-4)*f(n-4) +  f(n-5)*f(n-5)*f(n-5)
[define cache [make-vector 55 -1]]

(define (f n)
  (define (square x) (* x x))
  [define [cube x] [* x x x]]
  [if [<= n 4] 1
      [if [eq? [vector-ref cache n] -1]
      [let [[result [+ [f [- n 1]] [* 4 [f [- n 2]]] [* 5 [f [- n 3]]]
         [- [* 2 [square [f [- n 4]]]]]
         [cube [f [- n 5]]]]]]
        [begin [vector-set! cache n result] result]]
      [vector-ref cache n]]])

[define [main]
  [let [[a [read]]]
    [if [eq? a eof] [void]
        [begin [displayln [f a]] [main]]]]]

[main]
