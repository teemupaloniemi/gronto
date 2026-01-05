#lang racket

;; Reading from file.
(require json)


;; Read the courses from a given file.
(provide json-read)
(define (json-read filename)
    (call-with-input-file filename read-json))


;; Writing some course data in a file.
(provide json-write)
(define (json-write filename json)
  (call-with-output-file filename
    (λ (x) (write-json json x))
    #:exists 'replace))


;; Reading a string from SMT output.
;; Parsing this is done at dot.rkt -file.
(provide smt-read)
(define (smt-read filename)
  (port->string (open-input-file filename) #:close? #t))


;; Course following the questionnaire format.
(provide (struct-out course))
(struct course (name
                code
                credits
                periods
                prerequisite-courses
                skill-prerequisites
                skill-outcomes))


;; Node from the ontology paired with a bloom taxonomy weight (1-6)
(provide (struct-out ontology-node))
(struct ontology-node (name
                       bloom))


;; Pair of ontology nodes with a distance.
(provide (struct-out ontology-pair))
(struct ontology-pair (outcome
                       prerequisite
                       distance))

;; Pair of courses with a distance.
(provide (struct-out course-pair))
(struct course-pair (first
                     second
                     distance))

;; Map our JSON hash to course struct for readability.
(provide hash-to-struct)
(define (hash-to-struct h)
  (for/list ((c h))
    (course (hash-ref c 'title)
            (hash-ref c 'code)
            (hash-ref c 'credits)
            (hash-ref c 'periods)
            (hash-ref c 'course-prerequisites)
            (map (lambda (x) (ontology-node (car x)
                                            (cadr x)))
                 (hash-ref c 'skill-prerequisites))
            (map (lambda (x) (ontology-node (car x)
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
