#lang racket
(require "io.rkt")

;; General getter and subgetters.
(provide getField)
(define (getField elm field)
  (hash-ref elm field))

(provide getCode)
(define (getCode elm)
  (getField elm 'code))

(provide getCredits)
(define (getCredits elm)
  (getField elm 'credits))

;; Return a procedure for checking if an element has code equal to val.
(provide eq-code?)
(define (eq-code? val)
  (lambda (x) (string=? (getCode x) val)))

(provide getElmByCode)
(define (getElmByCode data code)
  (car (filter (eq-code? code) data)))


