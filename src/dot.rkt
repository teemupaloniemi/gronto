#lang racket

;; Reading from file.
(require "io.rkt")

;; Data queries.
(require "utils.rkt")

;; Parse the list of course code and semester pairs.
;; '(("ITKP104" "9") ("ITKA2030" "7") ("ITKP102" "3"))
(define (parse-smt-model filename)
  (let ((model (smt-read filename)))
    (map cdr
         (regexp-match* #px"sem_(\\w+)\\s*\\(\\)\\s*Int\\s+(\\d+)"
                        model
                        #:match-select values))))


(define (max-pairs sem-pairs)
  (max-list (map string->number (map cadr sem-pairs))))


(define (node pair courses)
  (let* ((code (car pair))
         (name (course-name (search-by-code courses code 'struct))))
    (display "            ")
    (display (car pair))
    (display " [label=\"")
    (display (car pair)) (display "\\n") (display name)
    (display "\"];")
    (newline)))


(define (subgraph sem sem-pairs courses)
  (display "    subgraph cluster_sem") (display sem) (display " {") (newline)
  (display "        label=\"Period ") (display sem) (display "\";") (newline)
  (display "        style=filled;") (newline)
  (display "        color=lightgray;") (newline)
  (display "        node [style=filled, fillcolor=lightblue];") (newline)
  (display "        sem_anchor_") (display sem)
  (display " [label=\"\", style=invisible, width=0, height=0];")
  (display "        { rank=same;") (newline)
  (map (lambda (p)
               (node p courses))
       (filter (lambda (c)
                       (= (string->number (cadr c)) sem))
               sem-pairs))
  (display "        }") (newline)
  (display "    }") (newline))


(define (anchor-subgraph sem)
  (display "    sem_anchor_")(display sem)
  (display " -> sem_anchor_")(display (+ 1 sem))
  (display " [style=invisible, arrowhead=none];")
  (newline))


(define (preq course preq)
  (display "    ") (display preq) (display " -> ") (display course) (newline))


(define (preqs-one course)
  (map (lambda (p)
               (preq (course-code course) p))
       (course-prerequisite-courses course)))


(define (preqs-all courses)
  (map preqs-one courses))


(define (year-subgraph year sem-pairs courses)
  (let ((sem-start (+ 1 (* year 4)))
        (sem-end (min (+ 4 (* year 4)) (max-pairs sem-pairs))))
    (display "    subgraph cluster_year") (display (+ year 1)) (display " {") (newline)
    (display "        label=\"Year ") (display (+ year 1)) (display "\";") (newline)
    (display "        style=filled; color=lightgreen;") (newline)
    (for-each
     (lambda (sem)
             (subgraph sem sem-pairs courses))
     (build-list (- (+ 1 sem-end) sem-start)
                 (lambda (i) (+ sem-start i))))
    (display "    }") (newline)))


(define (gen-dot courses sem-pairs)
  (let ((max-sem (max-pairs sem-pairs)))
    (define (num-years max-sem)
      (ceiling (/ max-sem 4)))
    (display "digraph Curriculum {") (newline)
    (display "    rankdir=TB") (newline)
    (display "    node [shape=box style=filled fillcolor=lightblue]") (newline)
    (map (lambda (y)
                 (year-subgraph y sem-pairs courses))
         (map (lambda (x) (- x 1))
              (range (num-years max-sem))))
    (map (lambda (s)
                 (anchor-subgraph s))
         (range (- max-sem 1)))
    (preqs-all courses)
    (display "}")))


(define (safe-gen-dot courses sem-pairs)
  (define (error-message)
    (raise "Error: could not parse z3 model"))
  (if (equal? sem-pairs '())
      (error-message)
      (gen-dot courses sem-pairs)))

(define (main)
  (safe-gen-dot (hash-to-struct (json-read "tmp/output.json"))
                (parse-smt-model "tmp/z3.txt")))

(main)
