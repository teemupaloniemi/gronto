#lang racket

;; Reading from file.
(require "io.rkt")

(provide (struct-out course))
(struct course (name code credits periods prerequisite-courses skill-prerequisites skill-outcomes))

;; Map our JSON hash to course struct for readability.
(provide hash-to-struct)
(define (hash-to-struct h)
  (for*/list ((c h))
    (course (hash-ref c 'title)
            (hash-ref c 'code)
            (hash-ref c 'credits)
            (hash-ref c 'periods)
            (hash-ref c 'course-prerequisites)
            (hash-ref c 'skill-prerequisites)
            (hash-ref c 'outcomes))))


;; Racket Generic Graph Library utilizes adjacency lists.
(provide hash-to-adjacency-lists)
(define (hash-to-adjacency-lists h)
  (let ((al '()))
    (define (recurse h prev prevprev)
      (if (hash? h)
         (if (eq? prevprev 'None)
           (set! al (cons (cons prev (hash-keys h)) al))
           (set! al (cons (cons prev (cons prevprev (hash-keys h))) al)))
         (set! al (cons (cons prev (list prevprev)) al)))
      (when (hash? h)
        (map (lambda (next)
                    (recurse (hash-ref h next)
                              next
                              prev))
            (hash-keys h))))
    (recurse h 'Root 'None)
    al))

;; Given a list of courses search those that match the given code.
(provide search-by-code)
(define (search-by-code courses code type)
  (define (first l)
    (if (eq? l '()) l (car l)))
  (if (equal? type 'hash)
    (first (filter (lambda (c) (equal? code (hash-ref c 'code))) courses))
    (first (filter (lambda (c) (equal? code (course-code c))) courses))))

;; Get a maximum of a list.
(define (max-element x y) (if (> x y) x y))
(provide max-list)
(define (max-list ls)
    (if (null? (cdr ls))
        (car ls)
        (max-element (car ls) (max-list (cdr ls)))))
