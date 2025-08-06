#lang racket

;; For graphviz.
(require graph)

;; Reading from file.
(require "io.rkt")

;; Data queries.
(require "utils.rkt")

;; Building and running the solver.
(require "smt.rkt")

(define (gen-dot courses sem-pairs)
  (define adj (map (lambda (c) (cons (course-code c) (course-prerequisite-courses c))) courses))
  (define g (unweighted-graph/adj adj))
  (graphviz g
            #:output (open-output-file "tmp/courses.dot")
            #:graph-attributes (list (list 'rankdir "TB"))))

(define (safe-gen-dot courses sem-pairs)
  (define (error-message)
    (raise "Error: could not solve for model"))
  (if (equal? sem-pairs '())
      (error-message)
      (gen-dot courses sem-pairs))
  #t)

(define (main)
  (define data (hash-to-struct (json-read "tmp/output.json")))
  (safe-gen-dot data (build-and-solve data 3 4 0 20)))

(main)
