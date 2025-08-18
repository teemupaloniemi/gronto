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

(define (gen-dot max-sem courses sem-pairs)
  (define (symbolic->string symbolic)
    (format "~s" symbolic))
  (define adj (map (lambda (c) (cons (course-code c)
                                     (course-prerequisite-courses c)))
                   courses))
  (define g (unweighted-graph/adj adj))
  (define-vertex-property g semester #:init "not-set")
  (define-vertex-property g id #:init "not-set")
  (define-vertex-property g label #:init "not-set")
  (for ((c sem-pairs))
       (semester-set! (symbolic->string (car c))
                      (cdr c))
       (id-set! (symbolic->string (car c))
                (symbolic->string (car c)))
       (label-set! (symbolic->string (car c))
                   (course-name (search-by-code courses
                                                (symbolic->string (car c))
                                                'struct))))
  (mygraphviz g
              id
              semester
              max-sem
              #t
              #:output (open-output-file "tmp/courses.dot"
                                         #:exists 'replace)
              #:graph-attributes (list (list 'rankdir "TB")
                                       (list 'ranksep 1))
              #:vertex-attributes (list (list 'semester semester)
                                        (list 'label label))))

(define (safe-gen-dot max-sem courses sem-pairs)
  (define (error-message)
    (raise "Error: could not solve for model"))
  (if (equal? sem-pairs '())
      (error-message)
      (gen-dot max-sem courses sem-pairs))
  #t)

(define (main)
  (define data (hash-to-struct (json-read "tmp/output.json")))
  (define years 1)
  (define sems 4)
  (define min-cred 0)
  (define max-cred 15)
  (define schedule (build-and-solve data years sems min-cred max-cred))
  (safe-gen-dot (* years sems) data schedule))

(main)
