#lang racket

;; Reading from file.
(require "io.rkt")

;; Data queries.
(require "utils.rkt")

;; Constant for edges that are infinite.
(define INF 9999)

;; Compute distance for each pair in cartesian product between
;; two lists using LCA path length style.
;;
;; Return a list of size (length l1) * (length l2).
(define (cp-path-lengths t l1 l2)
  (let ((cp (cartesian-product l1 l2)))

    ;; Return a path from root of a tree (t) to node (n) if exists.
    ;; Otherwise returns an empty list.
    (define (path-from-root t n)

      (define (create-path t n p)
        (cond
          ((not (hash? t)) '())
          ((member n (hash-keys t)) (reverse (cons n p)))
          (else
           (foldr (lambda (k acc)
                    (define sub (create-path (value k t) n (cons k p)))
                    (if (null? sub) acc sub))
                  '()
                  (hash-keys t)))))

      (create-path t n '()))

    ;; Distance from n1 to n2 in o is the length of
    ;;   path from root to n1
    ;; + path from root to n2
    ;; - (2 * common in the paths).
    (define (distance-nodes o n1 n2)
      (let ((p1 (path-from-root o n1))
            (p2 (path-from-root o n2)))

        ;; Return a list containing elements present in both l1 AND l2.
        (define (common l1 l2)
          (filter (lambda (x) (member x l1))
                  l2))
        (* (- (+ (length p1) (length p2))
              (* 2 (length (common p1 p2))))
           ;; Average depth of ontology nodes.
           ;; Deeper nodes convey more detailed
           ;; information.
           (/ (* (length p1) (length p2)) 2))))

    ;; Create a list of distances related to each node pair in
    ;; cartesian-product.
    (map (lambda (x)
                 (list (car x)
                       (cadr x)
                       (distance-nodes t
                                       (string->symbol (car x))
                                       (string->symbol (cadr x)))))
         cp)))


;; Distance between one course (c1) outcomes and another (c2) prerequisites
;; in ontology (t).
(provide distance)
(define (distance t c1 c2)
  (let* ((outs (value 'outcomes c1)) (louts (length outs))
         (pres (value 'outcomes c2)) (lpres (length pres))
         (weight1 (value 'credits c1))
         (weight2 (value 'credits c2)))

    ;; Distance between two partitions in ontology.
    (define (average-closest-neighbour-distance t outs pres)

        ;; Find closest neighbour of one outcome.
        (define (find-closest-neighbour o g)
          ;; f: Filter ones that have `o` as source.
          ;; s: Sort according to distance in acending order.
          (let* ((f (filter (lambda (x) (equal? (car x) o)) g))
                 (s (sort f (lambda (x y) (< (caddr x) (caddr y))))))
            (if (> (length f) 0)
                (caddar s)
                INF)))

        ;; Compute the cartesian prodict of outs and preds and
        ;; find closest-neighbour for all outcomes. By using
        ;; closest-neighbour tactic we gain the advantage of two
        ;; identical pratitions having no distance between them.
        (define closest-neighbours
          (let ((p (cp-path-lengths t outs pres)))
            (for*/list ((oi outs))
              (find-closest-neighbour oi p))))

        ;; Average distance betwee partitions usign the closests neighbours.
        (if (> (length closest-neighbours) 0)
            (/ (sum closest-neighbours) (length closest-neighbours))
            INF))

    ;; If ether is empty return large value.
    ;; We cant compute distance to nothing!
    (if (or (equal? outs 0) (equal? pres 0))
        INF
        (* weight1 weight2 (average-closest-neighbour-distance t outs pres)))))


;; Find the distance graph among the courses according to the distance function f.
;; Save results in triples:
;;     (src dst dist)
(provide G)
(define (G t f C)
    (map (lambda (p)
                 (list (value 'code (car p))
                       (value 'code (cadr p))
                       (f t (car p) (cadr p))))
         (cartesian-product C C)))


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
      (let ((src (value 'title (search-by-code courses (car p))))
            (dst (value 'title (search-by-code courses (cadr p))))
            (w (caddr p)))
        (display "    \"")
        (display src) (display "\" -> \"") (display dst)
        (display "\" [label=\"")
        (display (scaled w))
        (display "\" penwidth=2")
        (display ", color=")
        (display (color w))
        (display ", style=dashed")
        (display "];") (newline)))

    (display "digraph Distances {") (newline)
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


(define (main)
  (define courses (json-read "data/input.json"))
  (define ontology (json-read "data/acm.json"))
  (define u (G ontology distance courses))
  (define ũ (H u 666/1))

  (print-dot-graph ũ
                   courses)

  (save-results "tmp/output.json"
                ũ
                courses))

(main)
