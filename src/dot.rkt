#lang racket

;; For graphviz.
(require graph)
(require "graphviz.rkt")

;; Reading from file.
(require "io.rkt")

;; Data queries.
(require "utils.rkt")

;; Building and running the solver.
(require "smt.rkt")

(define (gen-dot courses sem-pairs)
  (define (symbolic->string symbolic)
    (format "~s" symbolic))
  (define adj (map (lambda (c) (cons (course-code c) (course-prerequisite-courses c))) courses))
  (define g (unweighted-graph/adj adj))
  (define-vertex-property g semester #:init "not-set")
  (define-vertex-property g id #:init "not-set")
  (for ((c sem-pairs))
       (semester-set! (symbolic->string (car c)) (cdr c))
       (id-set! (symbolic->string (car c)) (symbolic->string (car c))))
  (mygraphviz g
              id
              semester
              #:output (open-output-file "tmp/courses.dot"
                                         #:exists 'replace)
              #:graph-attributes (list (list 'rankdir "TB")
                                       (list 'ranksep 1))
              #:vertex-attributes (list (list 'semester semester))))

(define (safe-gen-dot courses sem-pairs)
  (define (error-message)
    (raise "Error: could not solve for model"))
  (if (equal? sem-pairs '())
      (error-message)
      (gen-dot courses sem-pairs))
  #t)

(define (main)
  (define data (hash-to-struct (json-read "tmp/output.json")))
  (define schedule (build-and-solve data 3 4 0 20))
  (safe-gen-dot data schedule))

(main)
