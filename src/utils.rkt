;; Utils, I know you hate these.
;; Thats why they are here!
#lang racket

;; Reading from file.
(require "io.rkt")

;; Create a range from 1 to n.
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


;; Return courses that match the given code.
(define (search-by-code data code)
  (safe-car (filter
              (lambda (x) (string=? (value 'code x) code))
                      data)))


;; Sum of the elements in a list.
(provide sum)
(define (sum l)
  (define (sum-r l s)
    (if (eq? '() l)
        s
        (sum-r (cdr l) (+ s (car l)))))
  (sum-r l 0))


;; Get a maximum of a list.
(define (max-element x y) (if (> x y) x y))
(provide max-list)
(define (max-list ls)
    (if (null? (cdr ls))
        (car ls)
        (max-element (car ls) (max-list (cdr ls)))))
