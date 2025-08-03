#lang racket

;; For shortest path algorithm.
(require graph)
;; Reading from file.
(require "io.rkt")
;; Data queries.
(require "utils.rkt")

;; Constant for edges that are infinite.
(define INF 9999)

(define (filter-distances apd l1 l2)
    (define (pair-distance a b)
      (if (equal? a b)
        0
        (value (list (string->symbol a) (string->symbol b)) apd)))
    (map (lambda (x)
                 (list (car x)
                       (cadr x)
                       (pair-distance (car x) (cadr x))))
         (cartesian-product l1 l2)))

;; Distance between one course (c1) outcomes and another (c2) prerequisites
;; in ontology (t).
(provide distance)
(define (distance all-pair-distances c1 c2)
  (let* ((outs (value 'outcomes c1))
         (pres (value 'outcomes c2))
         (weight1 (value 'credits c1))
         (weight2 (value 'credits c2)))

    ;; Distance between two partitions in ontology.
    (define (average-closest-neighbour-distance outs pres)

        ;; Find closest neighbour of one outcome.
        (define (find-closest-neighbour o g)
          (let* ((f (filter (lambda (x) (equal? (car x) o)) g))
                 (s (sort f (lambda (x y) (< (caddr x) (caddr y))))))
            (if (> (length f) 0)
                (caddar s)
                INF)))

        (define closest-neighbours
          ;; Each element in p has outcome, prerequisite and distance.
          ;; For example it might look like this:
          ;; (("Recursion"            "Graph theory" 60)
          ;;  ("Recursion"            "Logic"        35)
          ;;  ("Polymorphism"         "Graph theory" 60)
          ;;  ("Polymorphism"         "Logic"        35)
          ;;  ("Abstract data types"  "Graph theory" 60)
          ;;  ("Abstract data types"  "Logic"        35)
          ;;  ("Inheritance"          "Graph theory" 60)
          ;;  ("Inheritance"          "Logic"        35)
          ;;  ("Functional languages" "Graph theory" 60)
          ;;  ("Functional languages" "Logic"        35)
          ;;  ("Logic"                "Graph theory" 15)
          ;;  ("Logic"                "Logic" 0))
          (let ((p (filter-distances all-pair-distances outs pres)))
            (for*/list ((oi outs))
              ;; Here we find the minimum for each source.
              ;; For "Recursion" the closest neighbour is "Logic".
              ;; For "Functional languages" the closest neighbour is "Logic".
              ;; For "Logic" the closest neighbour is "Logic".
              ;; etc.
              ;; This is a bad example. Some could go also to graph theory.
              (find-closest-neighbour oi p))))

        ;; Average distance betwee partitions usign the closests neighbours.
        (if (> (length closest-neighbours) 0)
            (/ (sum closest-neighbours) (length closest-neighbours))
            INF))

    ;; If ether is empty return large value.
    ;; We cant compute distance to nothing!
    (if (or (equal? outs 0) (equal? pres 0))
        INF
        (* weight1 weight2 (average-closest-neighbour-distance outs pres)))))


;; Find the distance graph among the courses according to the distance function f.
;; Save results in triples:
;;     (src dst dist)
(provide G)
(define (G t f C)
    (let ((all-pair-distances (johnson (unweighted-graph/adj t))))
      (map (lambda (p)
                   (list (value 'code (car p))
                         (value 'code (cadr p))
                         (f all-pair-distances (car p) (cadr p))))
           (cartesian-product C C))))


;; Filter any edges below threshold value
;; and remove smaller of bidirectional edges.
(provide H)
(define (H u th)
  (define f (filter (lambda (x) (< (caddr x) th)) u))

  ;; Find the smallest of the two bidirectional edges between same nodes.
  (define (find-min-order current)

    ;; Find all that have the same codes but in different order.
    (define r (filter (lambda (new) (and (equal? (car  new) (cadr current))
                                         (equal? (cadr new) (car  current))))
                      f))

    ;; If some are found select the first.
    (define other
      (if (> (length r) 0)
         (car r)
         '()))

    ;; Find shorter.
    (if (not (equal? '() other))
        ;; Counterpart exists.
        (if (< (caddr current) (caddr other))
            ;; Current is the shortest direction.
            current
            ;; Oterwise they are equal or the other is shorter.
            (if (equal? (caddr current) (caddr other))
               ;; Equally short directions.
               ;;
               ;; If the codes are in lexical order use
               ;; current order so when we see the flipped
               ;; version it is _not_ in order and ordered
               ;; is again selected and thus can be removed
               ;; as duplicate later.
               (if (string>? (car current) (cadr current))
                   current
                   other)
               ;; The other is shorter direction.
               ;; Select that one.
               other))
        current))

  (remove-duplicates (for*/list ((p f))
                       (find-min-order p))))


;; Visualize :)
(define (print-dot-graph edges courses)
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
    (define (print-edge p)
      (let* ((src-course (search-by-code courses (car p)))
             (dst-course (search-by-code courses (cadr p)))
             (src-name (value 'title src-course))
             (dst-name (value 'title dst-course))
             (w (caddr p)))
        (display "    \"")
        (display src-name) (display "\" -> \"") (display dst-name)
        (display "\" [label=\"")
        (display (scaled w))
        (display "\" penwidth=2")
        (display ", color=")
        (display (color w))
        (display ", style=dashed")
        (display "];") (newline)))

    (display "digraph Distances {") (newline)
    (display "rankdir=TB;")
    (display "node [shape=box style=filled fillcolor=lightblue];") (newline)
    (map print-edge edges)
    (display "}") (newline)))


;; Assign prerequsite courses to courses and save them for scheduling.
(define (save-results filename graph courses)

  ;; Get a list of all known course codes.
  (define course-codes
    (map (lambda (x) (value 'code x))
         courses))

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
    (define mutable (hash-copy (search-by-code courses code)))
    (hash-set! mutable
               'course-prerequisites
               (get-prerequisites code))
    mutable)

  (json-write filename
              (map assign-prerequisites course-codes)))


;; Racket Generic Graph Library utilizes adjacency lists.
(define (hash-to-adjacency-lists h)
  (let ((al '()))
    (define (recurse h prev prevprev)
      (if (hash? h)
         (if (eq? prevprev 'None)
           (set! al (cons (cons prev (hash-keys h)) al))
           (set! al (cons (cons prev (cons prevprev (hash-keys h))) al)))
         (set! al (cons (cons prev (list prevprev)) al)))
      (when (hash? h)
        (map (lambda (next)
                    (recurse (hash-ref h next)
                              next
                              prev))
            (hash-keys h))))
    (recurse h 'Root 'None)
    al))


(define (main)
  (define courses (json-read "data/input.json"))
  (define ontology (hash-to-adjacency-lists (json-read "data/acm.json")))
  (define u (G ontology distance courses))
  (define ũ (H u 100/1))

  (print-dot-graph ũ courses)
  (save-results "tmp/output.json" ũ courses))

(main)
