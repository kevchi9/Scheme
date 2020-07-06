;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |ESERCIZIO 1|) (read-case-sensitive #t) (teachpacks ((lib "drawings.rkt" "installed-teachpacks") (lib "hanoi.rkt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.rkt" "installed-teachpacks") (lib "hanoi.rkt" "installed-teachpacks")) #f)))
(define sostantivo ;controlla la desinenza del sostantivo e capisce se si tratta di maschile/femminile, singolare/plurale
  (lambda (s)
    (cond ((string=? (substring s (- (string-length s) 1) (string-length s)) "o") "ms")
          ((string=? (substring s (- (string-length s) 1) (string-length s)) "i") "mp")
          ((string=? (substring s (- (string-length s) 1) (string-length s)) "a") "fs")
          (else "fp")
          )))
(define articolo ; in base al genere e alla quantità(ottenuti nella funziona sopra) associa il corretto articolo
  (lambda (s)
    (cond ((string=? (sostantivo s) "ms") "il")
          ((string=? (sostantivo s) "mp") "i")
          ((string=? (sostantivo s) "fs") "la")
          (else "le")
          )))
(define verbo  ; in base alla desinenza del verbo e del genere e quantità del sostantivo, coniuga in modo corretto il verbo 
  (lambda (v s)
    (cond ((string=? (substring v (- (string-length v) 3) (string-length v)) "are")
           (cond ((or (string=? (sostantivo s) "ms") (string=? (sostantivo s) "fs"))
                  (string-append (substring v 0 (- (string-length v) 3)) "a"))
                 (else
                  (string-append (substring v 0 (- (string-length v) 3)) "ano"))))
          (else
           (cond ((or (string=? (sostantivo s) "ms") (string=? (sostantivo s) "fs"))
                  (string-append (substring v 0 (- (string-length v) 3)) "e"))
                 (else
                  (string-append (substring v 0 (- (string-length v) 3)) "ono"))))
          )
    )
  )
(define frase
  (lambda (s v c)
    (string-append (articolo s)" " s  " "(verbo v s)" " (articolo c) " " c)
    )
  )