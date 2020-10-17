#lang racket

(define-syntax or
  (syntax-rules ()
    [(_) #true]
    [(_ x) x]
    [(_ x . xs)
     (if x x (or xs))]))
