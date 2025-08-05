#lang racket

;; Reading from file.
(require "io.rkt")

;; Data queries.
(require "utils.rkt")

;; For solvers.
(require rosette)
(require rosette/solver/smt/z3)

(define (declare solver courses sem-min sem-max min-cred-sem max-cred-sem)
  (let ((vars (for/list ((c courses))
                (cons c
                      (constant (datum->syntax #'courses
                                               (string->symbol (course-code c)))
                                integer?))))
        (sems (for/list ((x (in-range sem-min sem-max)))
                x)))

    ;; Assert semester limits to each course variable.
    (for ((v vars))
      (define/contract bs
                       (listof boolean?)
                       (list (and (>= (cdr v) sem-min)
                                  (<= (cdr v) sem-max))))
      (solver-assert solver bs))

    ;; Assert that prequisites come later that the course (if they exist).
    (for ([v1 vars])
       (for ([v2 vars])
           ;; When c2 is a member of c1 prerequisites
           (when (member (course-code (car v2))
                         (course-prerequisite-courses (car v1)))
             ;; c2 has to be taken before c1.
             (define/contract bs
                              (listof boolean?)
                              (list (> (cdr v1) (cdr v2))))
             (solver-assert solver bs))))

    ;; Check that each semester has at most n credits bound to it.
    (for ((s sems))
      (define/contract bs
                       (listof boolean?)
                       (list (> (apply + (for/list ((v vars))
                                           (constant (if (= (cdr v) s)
                                                     (course-credits (car v))
                                                     0)
                                               integer?)))
                                min-cred-sem)))
      (solver-assert solver bs))

    (for ((s sems))
      (define/contract bs
                       (listof boolean?)
                       (list (> (apply + (for/list ((v vars))
                                           (constant (if (= (cdr v) s)
                                                     (course-credits (car v))
                                                     0)
                                               integer?)))
                                min-cred-sem)))
      (solver-assert solver bs))

    solver))

(define (build-smt-model courses-path
                         years
                         sem-per-year
                         min-tot-cred
                         max-tot-cred
                         min-cred-sem
                         max-cred-sem)
  (let ((semesters (* years sem-per-year))
        (courses   (hash-to-struct (json-read courses-path)))
        (solver    (z3 'QF_LIA)))
    (define model (solver-check (declare solver courses 1 semesters min-cred-sem max-cred-sem)))
    (cons model (sat? model))))

(define (main)
  (build-smt-model "tmp/output.json"
                   3     ;; Years
                   4     ;; sem-per-year
                   180   ;; min-tot-cred
                   10000 ;; max-tot-cred
                   5     ;; min-sem-cred
                   20)) ;; max-sem-cred

(main)

