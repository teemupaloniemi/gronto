#lang racket

(require "io.rkt")

(provide getOntology)
(define (getOntology filename)
  (readfrom filename))
