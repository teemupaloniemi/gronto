#lang racket
(require json)

;; Sample data used for testing.
(define sample
   (list (hasheq 'title         "Tietokannat ja tiedonhallinta"
                 'code          "ITKA2004"
                 'credits       5
                 'department    "Informaatioteknologian tiedekunta"
                 'level         "Aineopinnot"
                 'outcomes      '()
                 'period        '()
                 'prerequisites '()
                 'years         '())
         (hasheq 'title         "Algoritmit 1"
                 'code          "ITKA201"
                 'credits       5
                 'department    "Informaatioteknologian tiedekunta"
                 'level         "Aineopinnot"
                 'outcomes      '()
                 'period        '()
                 'prerequisites '("ITKP102" "ITKA2004")
                 'years         '())))

;; Read the courses from a given file.
(define (readfrom filename)
    (call-with-input-file filename read-json))

;; Writing some course data in a file.
(define (writeto filename json)
  (call-with-output-file filename
    (λ (x) (write-json json x))
    #:exists 'replace))

(newline)
(pretty-print (readfrom "../tekka.json"))
(newline)

(writeto "test.json" sample)
(newline)
(pretty-print (readfrom "test.json"))
(newline)
