#lang racket

(require (only-in racket/base (eq? eqt?)))
(require racket/cmdline)
(require graph)
(require math/statistics)
(require rosette/safe)
(require rosette/solver/smt/z3)

(require "utils/graphviz.rkt")
(require "utils/utils.rkt")


(define (symbolic->string symbolic)
  (format "~s" symbolic))


(define (build-model solver courses sem-min sem-max min-cred-sem max-cred-sem)

        ;; List the semesters we need.
  (let ((semesters (stream->list (in-range sem-min
                                           (+ sem-max
                                              1))))

        ;; Initialize the solver symbolic constants.
        (vars (for/list ((c courses))
                        (cons c
                              (constant (course-code c)
                                        integer?)))))

    (for ((v1 vars))

      ;; Assert semester limits to each course variable.
      (define bs1 (list (and (>= (cdr v1) sem-min)
                             (<= (cdr v1) sem-max))))
      (solver-assert solver bs1)

      ;; If strict period constraints exists, apply them.
      ;; I.e course is kept in first semster of each year
      ;; and therefore to be in semester 1, 5 and 9.
      (when (not (equal? (course-periods (car v1)) '()))
        (for ((p (course-periods (car v1))))
             (solver-assert solver (list (equal? (cdr v1) p)))))

      ;; Assert that prequisites come later that the course (if they exist).
      (for ((v2 vars))

        ;; When c2 is a member of c1 prerequisites
        (when (member (course-code (car v1))
                      (course-prerequisite-courses (car v2)))

          ;; c2 has to be taken before c1.
          (define bs3 (list (> (cdr v2) (cdr v1))))
          (solver-assert solver bs3))))

    ;; Check that each semester has at most/least n credits bound to it.
    (for ((s semesters))
      (define bs (list (and (>= (apply + (for/list ((v vars))
                                                   (if (= (cdr v)
                                                          s)
                                                       (mean (course-credits (car v)))
                                                       0)))
                                min-cred-sem)
                            (<= (apply + (for/list ((v vars))
                                                   (if (= (cdr v)
                                                          s)
                                                       (mean (course-credits (car v)))
                                                       0)))
                                max-cred-sem))))
      (solver-assert solver
                     bs))

    solver))


(define (build-and-solve courses years sem-per-year min-cred-sem max-cred-sem)

  ;; Initilaize solver to use z3.
  (define solver (z3 'QF_LIA))

  ;; Construct the model with given data.
  (define mod (build-model solver
                           courses
                           1
                           (* years
                              sem-per-year)
                           min-cred-sem
                           max-cred-sem))

  ;; Check result.
  (define result (solver-check mod))

  ;; Return the schedule (model) and result.
  (hash->list (model result)))


(define (gen-dot courses max-sem sem-pairs outputport)

  ;; Construct an adjacency list of the the courses and their prerequisites.
  (define adj (map (lambda (c) (cons (course-code c)
                                     (course-prerequisite-courses c)))
                   courses))

  ;; Initialize graph
  (define g (unweighted-graph/adj adj))
  (define-vertex-property g
                          semester #:init
                          "not-set")
  (define-vertex-property g
                          id #:init
                          "not-set")
  (define-vertex-property g
                          label
                          #:init
                          "not-set")

  ;; Fill data
  (for ((pair sem-pairs))
    (let ((code (symbolic->string (car pair)))
          (semester (cdr pair)))
       (semester-set! code
                      semester)
       (id-set! code
                code)
       (label-set! code
                   (course-name (search-by-code courses
                                                code
                                                'struct)))))

  ;; Draw
  (mygraphviz g
              id
              semester
              max-sem
              #t
              #:output outputport
              #:graph-attributes (list (list 'rankdir "TB")
                                       (list 'ranksep 1))
              #:vertex-attributes (list (list 'semester semester)
                                        (list 'label label))))


(define (main args)

  ;; Parse command line arguments
  (define input-file (vector-ref args 0))
  (define graph-file (vector-ref args 1))
  (define years (string->number (vector-ref args 2)))
  (define sems (string->number (vector-ref args 3)))
  (define min-cred (string->number (vector-ref args 4)))
  (define max-cred (string->number (vector-ref args 5)))

  ;; Load data and initilaize IO
  (define courses (hash-to-struct (json-read input-file)))
  (define outputport (open-output-file graph-file
                                       #:exists
                                       'replace))

  ;; Do computation
  (define schedule (build-and-solve courses
                                    years
                                    sems
                                    min-cred
                                    max-cred))

  ;; Enjoy visualization
  (if (equal? schedule
              '())
      (displayln "error: \"schedule is empty, maybe solver failed?\"")
      (gen-dot courses
               (* years
                  sems)
               schedule
               outputport)))

(main (current-command-line-arguments))
