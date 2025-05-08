#lang racket
(require "io.rkt")

;; Utils, I know you hate these.
;; Thats why they are here!
(provide range)
(define (range n)
  (define (fill l n)
    (if (= n 0)
      l
      (fill (cons n l) (- n 1))))
  (fill '() n))

;; Get value of a filed corresponding the key in json elm.
(provide value)
(define (value key elm)
  (hash-ref elm key))

;; Given a list of json data search return those that match
;; the given code in a field "code".
(provide search-by-code)
(define (safe-car l)
  (if (eq? l '())
      l
      (car l)))

(define (search-by-code data code)
  (safe-car (filter
              (lambda (x) (string=? (value 'code x) code))
                data)))

;; Get a maximum of a list.
(define (max-element x y) (if (> x y) x y))
(provide max-list)
(define (max-list ls)
    (if (null? (cdr ls))
        (car ls)
        (max-element (car ls) (max-list (cdr ls)))))

