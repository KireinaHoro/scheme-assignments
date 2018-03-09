#lang racket

[define [square x] [* x x]]

[define [fast-pow base exp]
  [define [iter b n a]
    [cond [[= n 0] a]
          [[even? n]
           [iter [square b] [/ n 2] a]]
          [else
           [iter b [- n 1] [* a b]]]]]
  [iter base exp 1]]

[define [main]
  [let [[b [read]]
        [n [read]]]
    [if [eq? b eof]
        [void]
        [begin [displayln [fast-pow b n]] [main]]]]]

[main]