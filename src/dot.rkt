#lang racket
(require "io.rkt")
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
         (name (value 'title (search-by-code courses code))))
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
               (preq (value 'code course) p))
       (value 'prerequisites course)))

(define (preqs-all courses)
  (map preqs-one courses))

(define (gen-dot courses sem-pairs)
  (let ((max-sem (max-pairs sem-pairs)))
    (display "digraph Curriculum {") (newline)
    (display "    rankdir=TB") (newline)
    (display "    node [shape=box style=filled fillcolor=lightblue]") (newline)
    (map (lambda (s)
                 (subgraph s sem-pairs courses))
         (range max-sem))
    (map (lambda (s)
                 (anchor-subgraph s))
         (range (- max-sem 1)))
    (preqs-all courses)
    (display "}")))

(gen-dot (json-read "../data/tekka.json") (parse-smt-model "z3.txt"))

