#lang racket
(require "io.rkt")

;; Get value from element by key.
;; Similar to value = elm['key'] everywhere else.
(define (value key elm)
  (hash-ref elm key))

;; Search course by code.
(define (search-by-code data code)
  (car
    (filter
      (lambda (x) (string=? (value 'code x) code))
      data)))

;; Take two random courses and an ontology.
(define c1 (search-by-code (json-read "../data/tekka.json") "ITKP102"))
(define c2 (search-by-code (json-read "../data/tekka.json") "MATP1700"))
(define t (json-read "../data/acm.json"))

;; This is a dummy distance function.
(define (distance t c1 c2)
  (+ (value 'credit c1) (value 'credits c2)))

(newline)
(display (f t c1 c2))
(newline)
