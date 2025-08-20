#lang racket

;; For shortest path algorithm.
(require graph)
;; For saving precomputed data.
(require racket/serialize)
;; Mean
(require math/statistics)
;; Reading from file.
(require "io.rkt")
;; Data queries.
(require "utils.rkt")
;; For precomputed data.
(require "precomputed.rkt")
;; For the ontology.
(require "ontology.rkt")

;; Constant for edges that are infinite.
(define INF +inf.0)

;; Distance between one course (c1) outcomes and another (c2) prerequisites
;; in ontology (t) weighted by the course credits.
(provide distance)
(define (distance all-pair-distances outs pres weight1 weight2)
    ;; Self made heuristic on distance between two partitions in the ontology.
    (define (average-closest-neighbour-distance outs pres)

        ;; Select from all-pair-distances the ones that are relevant to given
        ;; courses.
        (define (filter-relevant-distances l1 l2)
            (define (pair-distance a b)
              (if (equal? a b)
                0
                (hash-ref all-pair-distances (list (string->symbol a) (string->symbol b)))))
            (define (make-pair-distance x)
              (list (caar x)
                    (caadr x)
                    (* (cadar x) (cadadr x) (pair-distance (caar x) (caadr x)))))
            (map make-pair-distance
                 (cartesian-product l1 l2)))

        ;; Find closest neighbour of all (c1) outcomes.
        (define closest-neighbours
          (let ((p (filter-relevant-distances outs pres)))
            ;; For each outcome (o) find the minimum in relevant pairs (p).
            ;; caddr is d (the precomputed distance) in the triple in (A B d).
            (for*/list ((o outs))
                      ;; Filter the ones that have o as the first key.
              (let* ((f (filter (lambda (x) (equal? (car x) (car o))) p))
                      ;; Sort them based on the precomputed distance.
                     (s (sort f (lambda (x y) (< (caddr x) (caddr y))))))
                ;; Return the precomputed distance of the first element if it
                ;; exists else return INF.
                (if (> (length f) 0)
                    (caddar s)
                    INF)))))

        ;; Average the results based on number of ourcomes.
        (if (> (length closest-neighbours) 0)
            (/ (foldr + 0 closest-neighbours) (length closest-neighbours))
            INF))

    ;; If ether is empty return large value.
    ;; We cant compute distance to nothing!
    (if (or (equal? outs 0) (equal? pres 0))
        INF
        (* weight1 weight2 (average-closest-neighbour-distance outs pres))))


;; Write data and return the data back.
(define (write-precomputed data)
  (displayln "Writing precomputed data.")
  (call-with-output-file "src/precomputed.rkt"
    (lambda (out)
      (fprintf out "#lang racket\n\n(provide precomputed)\n(define precomputed\n  ~s)\n" data))
    #:exists 'replace)
  data)

;; A wrapper for deciding which way we get the distance map.
;; If a precomputed file exists we load it. Otherwse the values
;; are computed, saved to a precomputed file and returned.
(define (get-all-pair-distances t)
    (if (hash? precomputed)
       precomputed
       (write-precomputed (johnson (unweighted-graph/adj t)))))

;; Find the distance graph among the courses according to the distance function f.
;; Save results in triples:
;;     (src dst dist)
(provide G)
(define (G t f C)
    ;; In theory this (computing the distance between all ontology items from
    ;; each other) should be in distance-function but because of bad design the
    ;; algorithm would run for every course pair and we dont want that.
    (let ((all-pair-distances (get-all-pair-distances t)))
      (map (lambda (p)
                   ;; For each pair
                   (list (course-code (car  p))
                         (course-code (cadr p))
                         ;; compute the distance.
                         (f all-pair-distances
                            (course-skill-outcomes (car  p))
                            (course-skill-prerequisites (cadr p))
                            (mean (course-credits (car  p)))
                            (mean (course-credits (cadr p))))))
           (cartesian-product C C))))


;; TODO: Could consider making bidirectionals a one component. If two courses
;; depend on each other make all future courses that depend on either one
;; depend on both of them and remove the bidirectional. This produces less cycles.
(provide H)
(define (H u th)
  ;; Filter any edges below threshold value.
  (define f (filter (lambda (x) (< (caddr x) th)) u))

  ;; Remove larger/worse/less prerequisite of bidirectional edges.
  (define (remove-bidirectionals current)
    (let* ((r (filter (lambda (new) (and (equal? (car  new) (cadr current))
                                         (equal? (cadr new) (car  current))))
                      f))
           (other (if (> (length r) 0) (car r) '())))

      ;; Find shorter. TODO: Make beautiful with cond. This is hard to read.
      (if (not (equal? '() other))
          (if (< (caddr current) (caddr other))
              ;; Current order is best.
              current
              (if (equal? (caddr current) (caddr other))
                 ;; Use name for predictable behaviour Since it does not really
                 ;; matter which way we choose.
                 (if (string>? (car current) (cadr current))
                     current
                     other)
                 other))
          ;; Only one order.
          current)))

  (remove-duplicates (for*/list ((p f))
                       (remove-bidirectionals p))))


;; Visualize :)
(define (print-dot-graph edges courses port)
  (parameterize ([current-output-port (open-output-file port #:exists 'replace)])
    (let* ((weights (map caddr edges))
           (min-weight (apply min weights))
           (max-weight (apply max weights)))

      ;; Normalized weight [0 (min) to 1 (max)]
      (define (scaled w)
        (/ (- w min-weight) (max 1 (- max-weight min-weight))))

      ;; Color based on original (non-inverted) scaled value
      (define (color w)
        (let ((s (scaled w)))
          (cond ((< s 0.50) "green")
                ((< s 0.75) "yellow")
                ((< s 0.85) "orange")
                (else "brown"))))

      ;; Print each edge with label, width, and color
      (define (conditional-print-edge p)
        (let* ((src-course (search-by-code courses (car p) 'struct))
               (dst-course (search-by-code courses (cadr p) 'struct))
               (src-name (course-name src-course))
               (dst-name (course-name dst-course))
               (w (caddr p)))
          (define (print-edge)
            (display "    \"")
            (display src-name) (display "\" -> \"") (display dst-name)
            (display "\" [label=\"")
            (display (scaled w))
            (display "\" penwidth=2")
            (display ", color=")
            (display (color w))
            (display ", style=dashed")
            (display "];")
            (newline))
          (when (not (equal? src-name dst-name))
            (print-edge))))

      (display "digraph Distances {") (newline)
      (display "label=\"Esitietoisuudet\";\n")
      (display "labelloc=\"t\";\n")
      (display "center=true;\n")
      (display "rankdir=BT;\n")
      (display "node [shape=box style=filled fillcolor=lightblue];\n") (newline)
      (map conditional-print-edge edges)
      (display "}") (newline))))


;; Assign prerequsite courses to courses and save them for scheduling.
;; TODO: Get rid of course-hashes.
(define (save-results filename graph course-hashes course-structs)
  ;; Get a list of all known course codes.
  (define course-codes
    (map (lambda (c) (course-code c))
         course-structs))

  ;; Find all edges that point to node with given code.
  (define (get-prerequisites code)
    (map car
         (filter (lambda (x) (and (not (eqv? (car x)
                                             code))
                                  (eqv? (cadr x)
                                        code)))
                 graph)))

  ;; Mutate the original course by adding a list of prerequisite-courses.
  (define (assign-prerequisites code)
    (define mutable (hash-copy (search-by-code course-hashes code 'hash)))
    (hash-set! mutable
               'course-prerequisites
               (get-prerequisites code))
    mutable)

  ;; Finally write to a file.
  (json-write filename (map assign-prerequisites course-codes)))


(define (main)
  ;; Courses hash is still needed as we will mutate and save them later.
  ;; Specifically we sill assign prerequisite courses based on our computation.
  (define course-hashes  (json-read "data/input.json"))
  (define course-structs (hash-to-struct course-hashes))
  ;; Ontology comes from import.
  (define u              (G ontology distance course-structs))
  (define ũ              (H u 1000/1))
  (when (> (length ũ) 0)
      (print-dot-graph ũ course-structs "tmp/distance.dot"))
  (save-results "tmp/output.json" ũ course-hashes course-structs))

(main)
