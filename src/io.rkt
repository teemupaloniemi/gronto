#lang racket
(require json)

(provide readfrom)
;; Read the courses from a given file.
(define (readfrom filename)
    (call-with-input-file filename read-json))

(provide writeto)
;; Writing some course data in a file.
(define (writeto filename json)
  (call-with-output-file filename
    (λ (x) (write-json json x))
    #:exists 'replace))

;; Sample data used for testing.
;; (define sample
;;   (list (hasheq 'title         "Tietokannat ja tiedonhallinta"
;;                 'code          "ITKA2004"
;;                 'credits       5
;;                 'department    "Informaatioteknologian tiedekunta"
;;                 'level         "Aineopinnot"
;;                 'outcomes      '()
;;                 'period        '()
;;                 'prerequisites '()
;;                 'years         '())
;;         (hasheq 'title         "Algoritmit 1"
;;                 'code          "ITKA201"
;;                 'credits       5
;;                 'department    "Informaatioteknologian tiedekunta"
;;                 'level         "Aineopinnot"
;;                 'outcomes      '()
;;                 'period        '()
;;                 'prerequisites '("ITKP102" "ITKA2004")
;;                 'years         '())))

;;(newline)
;;(pretty-print (readfrom "../data/tekka.json"))
;;(newline)

;;(writeto "../data/test.json" sample)
;;(newline)
;;(pretty-print (readfrom "../data/test.json"))
;;(newline)
