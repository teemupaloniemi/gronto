#lang racket
(require "io.rkt")
(require "course.rkt")
(require "ontology.rkt")

;; Take two random courses and an ontology.
(define c1 (getElmByCode (readfrom "../data/tekka.json") "ITKP102"))
(define c2 (getElmByCode (readfrom "../data/tekka.json") "MATP1700"))
(define ont (getOntology "../data/acm.json"))

;; (pretty-print c1)
;; (pretty-print c2)
;; (pretty-print ont)

;; This is a dummy distance function.
(define (f t c1 c2)
  (+ (getCredits c1) (getCredits c2)))

(display (f ont c1 c2))
(newline)
