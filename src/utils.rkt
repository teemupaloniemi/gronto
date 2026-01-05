#lang racket

;; Reading from file.
(require "io.rkt")

(provide (struct-out course))
(struct course (name
                code
                credits
                periods
                prerequisite-courses
                skill-prerequisites
                skill-outcomes))

(provide (struct-out skill))
(struct skill (name
               bloom))


;; Map our JSON hash to course struct for readability.
(provide hash-to-struct)
(define (hash-to-struct h)
  (for*/list ((c h))
    (course (hash-ref c 'title)
            (hash-ref c 'code)
            (hash-ref c 'credits)
            (hash-ref c 'periods)
            (hash-ref c 'course-prerequisites)
            (map (lambda (x) (skill (car x)
                                    (cadr x)))
                 (hash-ref c 'skill-prerequisites))
            (map (lambda (x) (skill (car x)
                                    (cadr x)))
                 (hash-ref c 'outcomes)))))


;; Given a list of courses search those that match the given code.
(provide search-by-code)
(define (search-by-code courses code type)
  (define (first l)
    (if (eq? l '()) l (car l)))
  (if (equal? type 'hash)
    (first (filter (lambda (c) (equal? code (hash-ref c 'code))) courses))
    (first (filter (lambda (c) (equal? code (course-code c))) courses))))
