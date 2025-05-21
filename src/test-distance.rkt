#lang racket

;; Reading from file.
(require "io.rkt")

;; Data queries.
(require "utils.rkt")

;; Functions that we test.
(require "distance.rkt")


;; Colors for pretty printing.
(define OKGREEN "\033[92m")
(define WARNING "\033[93m")
(define FAIL "\033[91m")
(define ENDC "\033[0m")


;; Data
(define COURSES (json-read "data/input.json"))
(define ONTOLOGY (json-read "data/acm.json"))


;; Test utility function. We give two pairs of
;; courses codes and check if the distance between
;; first pair is larger than the first. If so we
;; print success message otherwise error is displayed.
(define (is-larger-distance? p1 p2)
  (let ((c11 (search-by-code COURSES (car p1)))
        (c12 (search-by-code COURSES (cadr p1)))
        (c21 (search-by-code COURSES (car p2)))
        (c22 (search-by-code COURSES (cadr p2))))
    (define (error-message)
        (display FAIL)
        (display "Comaprison failed:") (newline)
        (display WARNING)
        (display "  |")
        (display (value 'title c11))
        (display "|")
        (display " TO ")
        (display "|")
        (display (value 'title c12))
        (display "|")
        (display FAIL)
        (display " <= ")
        (display ENDC)
        (display WARNING)
        (display "|")
        (display (value 'title c21))
        (display "|")
        (display " TO ")
        (display "|")
        (display (value 'title c22))
        (display "|")
        (display FAIL)
        (newline) (display "which was not expected!") (newline))
        (display ENDC)
    (define (success)
        (display OKGREEN)
        (display "OK: |")
        (display (value 'code c11))
        (display "|")
        (display " TO ")
        (display "|")
        (display (value 'code c12))
        (display "|")
        (display " > ")
        (display "|")
        (display (value 'code c21))
        (display "|")
        (display " TO ")
        (display "|")
        (display (value 'code c22))
        (display "|")
        (newline)
        (display ENDC))
    (if (> (distance ONTOLOGY c11 c12)
           (distance ONTOLOGY c21 c22))
        (success)
        (error-message))))


;; These are heuristic tests so we can be sure that
;; testing new distance functions does not break anything.
;;
;; (ohj1 - olio) > (ohj1 - fun1)
(is-larger-distance? '("ITKP102" "TIEA1130")
                     '("ITKP102" "TIEA341"))
;; (fun1 - joukko-oppi_ja_graafit
;; >
;; (ohj1 - joukko-oppi_ja_graafit)
(is-larger-distance? '("TIEA341" "MATA2520")
                     '("ITKP102" "TIEA341"))
;; (alg1 - fun1) > (alg1 - ohj1)
(is-larger-distance? '("TIEA341" "MATA2520")
                     '("ITKP102" "TIEA341"))
