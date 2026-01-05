#lang racket

(require racket/cmdline)
(require graph)
(require racket/serialize)
(require math/statistics)

(require "utils.rkt")
(require "precomputed.rkt")
(require "ontology.rkt")
(require "graphviz.rkt")


(define INF +inf.0)


;; Writes the hash for next compilation to use.
;; Returns:
;;   The written hash for this run to use.
(define (write-precomputed data)
  (call-with-output-file "src/precomputed.rkt"
    (lambda (out)
      (fprintf out "#lang racket\n\n(provide precomputed)\n(define precomputed\n  ~s)\n" data))
    #:exists 'replace)
  data)


;; Global variable where all ontology pairs and their distances are computed.
(define all-pair-distances
  (if (hash? precomputed)
      precomputed
      (write-precomputed (johnson (unweighted-graph/adj ontology)))))


;; ontology-distance : (N x N) --> Integer
;; Returns:
;;    A precomputed value from hash matching the arguments.
(define (ontology-distance outcome prerequisite)
  (+ 1
     (hash-ref all-pair-distances
               (list (string->symbol outcome)
                     (string->symbol prerequisite)))))


;; After this there are no two nodes that have arrows pointing to each other.
;; Which means that one of them is removed. Heuristic is "shortest arrow stays".
(define (remove-bidirectionals graph x)

  ;; Find the reciprocal of "x" and name it "y".
  (let ((y (findf (lambda (ŷ) (and (equal? (course-pair-first ŷ)
                                           (course-pair-second x))
                                   (equal? (course-pair-second ŷ)
                                           (course-pair-first x))))
                                 graph)))

    ;; Find the shortest of the two.
    (cond
          ;; no reciprocal found, x is the only one
          ((not y)
           x)

          ;; x is shorter
          ((< (course-pair-distance x)
              (course-pair-distance y))
           x)

          ;; y is shorter
          ((< (course-pair-distance y)
              (course-pair-distance x))
           y)

          ;; x and y are equal, alphabetical order is tiebraker
          ((string>? (course-pair-first  x)
                     (course-pair-second x))
           x)

          ;; Keep the Racketnisse happy, as no computation is meaningless!
          (else y))))


;; H : (N x N x Q)* x Q --> (N x N x Q)*
;; Returns:
;;   Filtered graph such that courses that are barely related are disconnected
;;   and (in case of two directional edges) only shortest distance is preserved.
(define (H graph threshold)

  ;; filter arrows that are above the threshold.
  (let ((filtered-graph (filter (lambda (x) (< (course-pair-distance x)
                                               threshold))
                                graph)))

    ;; Remove bidirectional arrows.
    (remove-duplicates (for/list ((fgi filtered-graph))
                         (remove-bidirectionals filtered-graph
                                                fgi)))))


;; get-prerequisites : (N x N x Q)* x code --> code*
;; Returns:
;;   List of codes that are prerequisites to current code.
(define (get-prerequisites graph code)

  ;; Find such arrows that have current "code" as the end point (second).
  (let ((filtered-graph (filter (lambda (x) (and (not (equal? (course-pair-first x)
                                                              code))
                                                 (equal? (course-pair-second x)
                                                         code)))
                                graph)))

    ;; Return the start point of those arrows.
    (map course-pair-first
         filtered-graph)))


;; mutate-prerequisites : C* x code x (code x code x Q) --> C
(define (mutate-prerequisites course-hashes code graph)
  (let ((mutable (hash-copy (search-by-code course-hashes
                                            code
                                            'hash))))
    (hash-set! mutable
               'course-prerequisites
               (get-prerequisites graph
                                  code))
    mutable))


(define (save-results filename graph course-hashes course-structs)
  (json-write filename
              (map (lambda (c) (mutate-prerequisites course-hashes
                                                     (course-code c)
                                                     graph))
                   course-structs)))


;; bloom-difference-weight : ([1..6] x [1..6]) --> ([1/6..6/6])
;; Returns:
;;   Rational weight describing an arrow between the outcome and prerequisite.
(define (bloom-difference-weight outcome-bloom prerequisite-bloom)
  (if (< outcome-bloom
         prerequisite-bloom)
      (let ((m 6)) ;; The maximum difference is 6.
        (/ (- m
              (- prerequisite-bloom
                 outcome-bloom))
           m))
      1))


;; shortest-pair : (N x N x Q)* x N --> Q
;; Returns:
;;   Shortest of the triplets containing "one".
(define (shortest-pair ontology-pairs one)

  ;; Consider only pairs with "one" in outcomes.
  (define filtered-pairs (filter (lambda (p) (equal? (ontology-pair-outcome p)
                                                     (ontology-node-name one)))
                                 ontology-pairs))

  ;; Sort them in "shortest first" order.
  (define sorted-pairs (sort filtered-pairs
                             (lambda (x y) (< (ontology-pair-distance x))
                                              (ontology-pair-distance y))))

  ;; Return the shortest (which is first).
  (if (equal? sorted-pairs '())
      INF
      (ontology-pair-distance (car sorted-pairs))))


;; f : (N x W) x (N x W) --> Q
;; Returns:
;;   Distance between the two ontology nodes.
(define (f outcome prerequisite)
  (* (bloom-difference-weight (ontology-node-bloom outcome)
                              (ontology-node-bloom prerequisite))
     (ontology-distance (ontology-node-name outcome)
                        (ontology-node-name prerequisite))))


;; fs : (N x W)* x (N x W)* --> (N x N x Q)*
;; Returns:
;;   Distances between pairs of ontology nodes (with associated nodes).
(define (fs outcomes prerequisites)
  (map (lambda (p) (ontology-pair (ontology-node-name (car p))
                                  (ontology-node-name (cadr p))
                                  (f (car p)
                                     (cadr p))))
       (cartesian-product outcomes
                          prerequisites)))


;; closest : (N x W)* x (N x W)* --> Q*
;; Returns:
;;   List of distances (one for each outcome).
(define (closest outcomes prerequisites)

  ;; Get all distances.
  (define distances (fs outcomes
                        prerequisites))

  ;; Return the shortest for each outcome.
  (map (lambda (o) (shortest-pair distances
                                  o))
       outcomes))


;; G : (N x W)* x (N x W)* --> Q
;; Returns:
;;   Distance between two lists of weighted ontology nodes.
(define (G outcomes prerequisites)
  (if (and (> (length outcomes)
              0)
           (> (length prerequisites)
              0))
      (mean (closest outcomes
                     prerequisites))
      INF))


;; D : C x C --> (code x code x Q)
;; Returns:
;;  Distance between two courses (and names associated).
(define (D course-1 course-2)
  (course-pair (course-code course-1)
               (course-code course-2)
               (* (mean (mean (course-credits course-1))
                        (mean (course-credits course-2)))
                  (G (course-skill-outcomes course-1)
                     (course-skill-prerequisites course-2)))))


;; map-D : C* --> (code x code x Q)*
;; Returns:
;;  Distance between all courses. List of triples with two codes and a distance.
(define (map-D course-structs)
  (map (lambda (p) (D (car p)
                      (cadr p)))
       (cartesian-product course-structs
                          course-structs)))


(define (main args)

  ;; Command line arguments
  (define input-file       (vector-ref args 0))
  (define graph-file       (vector-ref args 1))
  (define output-file      (vector-ref args 2))
  (define filter-threshold (string->number (vector-ref args 3)))

  ;; Data
  (define course-hashes  (json-read input-file))
  (define course-structs (hash-to-struct course-hashes))

  ;; Computation
  (define graph (map-D course-structs))
  (define filtered-graph (H graph
                            filter-threshold))

  ;; Visualization
  (when (> (length filtered-graph)
           0)
      (print-dot-graph filtered-graph
                       course-structs
                       graph-file))

  ;; Saving results
  (save-results output-file
                filtered-graph
                course-hashes
                course-structs))


(main (current-command-line-arguments))
