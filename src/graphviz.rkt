;; This file is a derivative work, specifically modified document, from
;; https://github.com/stchang/graph and is licenced under Apache License
;; Version 2.0, January 2004 which can be found from
;; http://www.apache.org/licenses/
#lang racket/base

(require racket/format
         (only-in racket/list
                  remove-duplicates)
         racket/port
         racket/pretty
         racket/set
         racket/string
         racket/unsafe/ops
         graph)
(require "utils.rkt")

(provide mygraphviz)

(define-syntax-rule (first x) (unsafe-car x))
(define-syntax-rule (second x) (unsafe-car (unsafe-cdr x)))

(define (sanitize-name name)
  (cond
    [(string? name) name]
    [(symbol? name) (symbol->string name)]
    [(number? name) (number->string name)]
    [else (pretty-format name)]))

(define (color-attr colors color-count v)
  (if (and color-count (hash-ref colors v #f))
      (let* ([percent (/ (hash-ref colors v #f) color-count)]
             [str (~a #:max-width 5 (exact->inexact percent))]
             [triple (format "~a 1.0 1.0" str)])
        `([color ,triple]))
      null))

(define (weight-attr weighted? g e)
  (if weighted?
      (let ([weight (edge-weight g (first e) (second e))])
        `([label ,(sanitize-name weight)]))
      null))

(define (vertex-attrs-get-val attrs v)
  (for/list ([attr (in-list attrs)])
    (define get (cadr attr))
    (list (car attr) (get v))))

(define (edge-attrs-get-val attrs e)
  (for/list ([attr (in-list attrs)])
    (define get (cadr attr))
    (list (car attr) (get (first e) (second e)))))

(define (attrs->string attrs [sep ","] [after-last ""])
  (define attrs* (remove-duplicates attrs #:key car))
  (define attr-strs
    (for/list ([attr (in-list attrs*)])
      (format "~a=~s" (car attr) (cadr attr))))
  (string-join attr-strs sep #:after-last after-last))

;; Return a graphviz definition for a graph
;; Pass a hash of vertex -> exact-nonnegative-integer? as coloring to color the nodes
(define (mygraphviz g
                    id
                    rank
                    nsubs
                    draw-arrows
                  #:colors [colors #f]
                  #:graph-attributes [graph-attrs null]
                  #:edge-attributes [edge-attrs null]
                  #:vertex-attributes [vertex-attrs null]
                  #:output [port #f])
  (define (generate-graph)
    (parameterize ([current-output-port (or port (current-output-port))])
      (define weighted? (weighted-graph? g))
      (define node-count 0)
      (define node-id-table (make-hash))
      (define (node-id-table-ref! node)
        (hash-ref! node-id-table node
                   (λ ()
                     (begin0 (format "node~a" node-count)
                       (set! node-count (add1 node-count))))))
      (printf "digraph G {\n")
      (printf "label=\"Aikataulu\";\n")
      (printf "labelloc=\"t\";\n")
      (printf "center=true;\n")

      (when (not (null? graph-attrs))
        (printf "\t~a" (attrs->string graph-attrs ";\n" ";\n")))
      (printf "        node [shape=box style=filled fillcolor=lightblue];\n")
      ; Add vertices, color them using evenly spaced HSV colors if given colors
      (define color-count (and colors (add1 (apply max (hash-values colors)))))
      (for ([v (in-vertices g)])
        (define attrs
          (append (vertex-attrs-get-val vertex-attrs v)
                  (color-attr colors color-count v)))
        (printf "\t~a [~a];\n"
                (id v #:default (node-id-table-ref! v))
                (attrs->string attrs)))

      ;; Create grouped subgraphs for each semester.
      (define subs (build-list nsubs (lambda (x) (+ 1 x))))
      (for ((s subs))
           (printf "        subgraph cluster_sem~a { graph[bgcolor=white, style=dotted, label=\"Periodi ~a\"];" s s)
           (map (lambda (v) (printf " ~a; " (id v)))
                (filter (lambda (c) (= (rank c) s))
                        (get-vertices g)))
           (printf " {rank=\"same\"")
           (printf " h~a [style=invisible, fixedsize=true, width=0.1, height=0.1]; t~a [style=invisible, fixedsize=true, width=0.1, height=0.1]; " s s)
           (map (lambda (v) (printf " ~a; " (id v)))
                (filter (lambda (c) (= (rank c) s))
                        (get-vertices g)))
           (printf "}}\n"))

      (for ((s subs))
          (when (< s nsubs)
              (printf "t~a -> h~a [style=invisible, arrowhead=none];\n" s (+ 1 s))))

      ; Write undirected edges as one subgraph
      (printf "  subgraph U {\n")
      (printf "    edge [dir=none];\n")
      (define undirected-edges
        (for/fold ([added (set)])
                  ([e (in-edges g)]
                   #:when (and (not (set-member? added e))
                               (has-edge? g (second e) (first e))
                               (equal? (edge-weight g (first e) (second e))
                                       (edge-weight g (second e) (first e)))))
          (define attrs
            (append (edge-attrs-get-val edge-attrs e)
                    (weight-attr weighted? g e)))
          (printf "    ~a -> ~a [~a];\n"
                  (node-id-table-ref! (first e))
                  (node-id-table-ref! (second e))
                  (attrs->string attrs))
          (set-add (set-add added e) (list (second e) (first e)))))
      (printf "  }\n")

      ; Write directed edges as another subgraph
      (when draw-arrows
        (printf "  subgraph D {\n")
        (for ([e (in-edges g)] #:unless (set-member? undirected-edges e))
          (define attrs
            (append (edge-attrs-get-val edge-attrs e)
                    (weight-attr weighted? g e)))
          (printf "    ~a -> ~a [~a];\n"
                  (second e)
                  (first e)
                  (attrs->string attrs)))
        (printf "  }\n"))
      (printf "}\n")))

  (if port
      (generate-graph)
      (with-output-to-string generate-graph)))


(provide print-dot-graph)
(define (print-dot-graph edges courses port)
  ;; Print each edge with label, width, and color
  (parameterize ([current-output-port (or (open-output-file port #:exists 'replace) (current-output-port))])
    (display "digraph distances {")
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
          (display w)
          (display "\"")
          (display ", style=dashed")
          (display "];")
          (newline))
        (when (not (equal? src-name dst-name))
          (print-edge))))

    (map conditional-print-edge edges)
    (display "}")))
