#lang racket

(require racket/cmdline)
(require racket/serialize)
(require math/statistics)

; (require graph)

(require "utils/utils.rkt")
(require "utils/ontology.rkt")
(require "utils/graphviz.rkt")

;; Briefly on notation I use in comments
;;
;;   Functions
;;
;;     <name> : <input type> --> <output-type>
;;
;;   Primitive Types
;;
;;     I             integers
;;     Q             rational numbers
;;     S             strings
;;
;;   Compound Types
;;
;;     O             ontology items
;;     C             courses
;;     OP            ontology pair
;;     CP            course pair (the final graph will be of type CP*)
;;
;;   Operators
;;
;;     <T>*          arbitrary length list of items of type "T"
;;     <T1> x <T2>   cartesian product of the two types
;;
;; From here on,
;; the recommended direction for reading this file is "bottom to top".


;; Global constant representing infinity and epsilon.
(define INFINITY +inf.0)
(define EPSILON (/ 1 10000))


;; Recursively traverse the graph from src until tgt is found.
(define (recursive-path G src tgt)

  (define (get-adj G s)
    (let ((a (filter (lambda (x) (equal? (car x) s))
                     G)))
      (when (equal? a '())
        (display s)
        (displayln " is missing from the ontology. Check input!"))

      (if (equal? a '())
          '()
          (car a))))

  (define (recursive-path-inner current visited count)
    (cond ;; Empty list
          ((equal? current '()) #f)
          ;; Already visited
          ((member (car current) visited) #f)
          ;; Target found
          ((member tgt current) (cons tgt (cons (car current) visited)))
          ;; Search to child nodes
          (else (ormap (lambda (c) (recursive-path-inner (get-adj G
                                                                  c)
                                                         (cons (car current)
                                                               visited)
                                                         (+ count 1)))
                       (cdr current)))))

  (recursive-path-inner (get-adj G
                                 src)
                        '()
                        0))


;; Return only common items in both l1 and l2.
(define (common-list l1 l2)
  (filter (lambda (c) (member c l2))
          l1))


;; Return only not common items beween l1 and l2.
(define (not-common-list l1 l2)
  (filter (lambda (c) (not (member c l2)))
          l1))


;; Sum path lengths from src to root and tgt to root and subract the common
;; part that was counted twice.
(define (first-recursive-path G src tgt)
  (let (;; Source to root
        (srcp (recursive-path ontology src (string->symbol "Root")))
        ;; Target to root
        (tgtp (recursive-path ontology tgt (string->symbol "Root"))))
     (flatten (list (reverse (not-common-list srcp tgtp))
                    (last (common-list srcp tgtp))
                    (not-common-list tgtp srcp)))))


;; ontology-distance : O x O --> I
;; Returns:
;;    Shortest path length between outcome and prerequisite in number of nodes.
(define (ontology-distance outcome prerequisite)
  (length (first-recursive-path ontology
                                (string->symbol (ontology-node-name outcome))
                                (string->symbol (ontology-node-name prerequisite)))))

  ;; Test against slow library implementation.
  ;(let ((a (first-recursive-path ontology
  ;                               (string->symbol (ontology-node-name outcome))
  ;                               (string->symbol (ontology-node-name prerequisite))))
  ;      (b (fewest-vertices-path (unweighted-graph/adj ontology)
  ;                               (string->symbol (ontology-node-name outcome))
  ;                               (string->symbol (ontology-node-name prerequisite)))))

  ;  (when (not (equal? (length a)
  ;                     (length b)))
  ;    (display (length a))
  ;    (display " != ")
  ;    (displayln (length b))
  ;    (displayln a)
  ;    (displayln b))

  ;  (length a)))


;; remove-bidirectional : CP* x CP --> CP
;; Returns:
;;   Shorter of the arrows x and (reciprocal x).
(define (remove-bidirectional graph x)

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


;; H : OP* x Q --> OP*
;; Returns:
;;   Filtered graph such that courses that are barely related are disconnected
;;   and (in case of two directional edges) only shortest distance is preserved.
(define (H graph threshold)

  ;; filter arrows that are below the threshold.
  (let ((filtered-graph (filter (lambda (x) (< (course-pair-distance x)
                                               threshold))
                                graph)))

    ;; Remove bidirectional arrows. "remove-bidirectional"-function returns
    ;; the shortest arrow of the two and will result in duplicate arrows.
    (remove-duplicates (map (lambda (x) (remove-bidirectional filtered-graph
                                                              x))
                            filtered-graph))))


;; get-prerequisites : CP* x S --> S*
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


;; mutate-prerequisites : C* x S x CP* --> C
;; Returns:
;;  A version of course (with code "code") that has newly assigned prerequisite
;;  courses.
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


;; bloom-difference-weight : O x O --> (1/5 <= Q <= 5/5)
;; Returns:
;;   Rational weight describing an arrow between the outcome and prerequisite.
(define (bloom-difference-weight outcome prerequisite)
  (* (ontology-node-bloom outcome)
     (ontology-node-bloom prerequisite)))
  ;(if (< (ontology-node-bloom outcome)
  ;       (ontology-node-bloom prerequisite))
  ;    (- 1
  ;       (/ (- (ontology-node-bloom prerequisite)
  ;             (ontology-node-bloom outcome))
  ;          5))
  ;    1))


;; shortest-pair-distance : OP* x O --> Q
;; Returns:
;;   Shortest of the ontology-pairs containing "one" as outcome.
(define (shortest-pair-distance ontology-pairs one)

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
      INFINITY
      (ontology-pair-distance (car sorted-pairs))))


;; f : O x O --> Q
;; Returns:
;;   Distance between the two ontology nodes.
(define (f outcome prerequisite)
  (* (bloom-difference-weight outcome
                              prerequisite)
     (ontology-distance outcome
                        prerequisite)))


;; fs : O* x O* --> OP*
;; Returns:
;;   Distances between pairs of ontology nodes (with associated nodes).
(define (fs outcomes prerequisites)
  (map (lambda (p) (ontology-pair (ontology-node-name (car p))
                                  (ontology-node-name (cadr p))
                                  (f (car p)
                                     (cadr p))))
       (cartesian-product outcomes
                          prerequisites)))


;; harm : N* --> Q
;; Returns:
;;   Harmonic mean of a list of integers.
(define (harm l1)
  (define (inverse x) (/ 1 x))
  (let ((a (foldl  +
                   0
                   (map (lambda (x) (inverse (+ x
                                                EPSILON)))
                        l1))))
    (if (equal? a 0)
      INFINITY
      (/ 1
         (/ a
            (length l1))))))


;; closest : O* x O* --> Q*
;; Returns:
;;   List of distances (one for each outcome).
(define (closest outcomes prerequisites)

  ;; Get all distances.
  (define ontology-pairs (fs outcomes
                             prerequisites))

  ;; Return the shortest for each outcome "o".
  (map (lambda (o) (shortest-pair-distance ontology-pairs
                                           o))
       outcomes))


;; G : O* x O* --> Q
;; Returns:
;;   Distance between two lists of weighted ontology nodes.
(define (G outcomes prerequisites)
  (if (and (> (length outcomes)
              0)
           (> (length prerequisites)
              0))
      (harm (closest outcomes
                     prerequisites))
      INFINITY))


;; D : C x C --> CP
;; Returns:
;;  Distance between two courses (and names associated).
(define (D course-1 course-2)
  (course-pair (course-code course-1)
               (course-code course-2)
               (* (/ 1
                     (* (mean (course-credits course-1) #f)
                        (mean (course-credits course-2) #f)))
                  (G (course-skill-outcomes course-1)
                     (course-skill-prerequisites course-2)))))


;; map-D : C* --> CP*
;; Returns:
;;  Distance between all courses. List of triples with two codes and a distance.
(define (map-D course-structs)
  (map (lambda (p) (D (car p)
                      (cadr p)))
       (cartesian-product course-structs
                          course-structs)))


(define (main args)

  ;; Parse command line arguments
  (define input-file       (vector-ref args 0))
  (define graph-file       (vector-ref args 1))
  (define output-file      (vector-ref args 2))
  (define filter-threshold (string->number (vector-ref args 3)))

  ;; Load data
  (define course-hashes  (json-read input-file))
  (define course-structs (hash-to-struct course-hashes))

  ;; Do computation
  (define graph (map-D course-structs))
  (define filtered-graph (H graph
                            filter-threshold))

  ;; Enjoy visualization
  (if (equal? filtered-graph
              '())
      (displayln "error: \"prerequisite-graph is empty, maybe check data or adjust threshold?\"")
      (print-dot-graph filtered-graph
                       course-structs
                       graph-file))

  ;; Save results
  (save-results output-file
                filtered-graph
                course-hashes
                course-structs))

(main (current-command-line-arguments))
