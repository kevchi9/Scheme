;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |ESERCIZIO 4|) (read-case-sensitive #t) (teachpacks ((lib "drawings.rkt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.rkt" "installed-teachpacks")) #f)))
(define lsd ; val: chr -> restituisce il carattere meno significativo
  (lambda (s)
    (if (string=? s "")
        #\.
        (string-ref s (-(string-length s) 1)))
    )
  )
(define head ; val: String -> restituisce la parte prima del lsd
  (lambda (s)
    (if (string=? s "")
        ""
        (substring s 0 (- (string-length s) 1))
    )
  ))
(define normalized-btr ; val: String -> restituisce la Stringa di partenza ma rimuovendo i punti in testa
  (lambda (s)
    (if (string=? s "")
        "."
        (if (char=? (string-ref s 0) #\.)
            (normalized-btr (head s))
            s)
        )
    )
  )
; btr-digit-sum esegue tutte le somme di btr
(define btr-digit-sum                    ; val:     carattere +/./-
  (lambda (u v c)                        ; u, v, c: caratteri +/./-
    (cond ((char=? u #\-)                ; u v c
           (cond ((char=? v #\-)
                  (cond ((char=? c #\-)  ; - - -
                         #\.)
                        ((char=? c #\.)  ; - - .
                         #\+)
                        ((char=? c #\+)  ; - - +
                         #\-)))
                 ((char=? v #\.)
                  (cond ((char=? c #\-)  ; - . -
                         #\+)
                        ((char=? c #\.)  ; - . .
                         #\-)
                        ((char=? c #\+)  ; - . +
                         #\.)))
                 ((char=? v #\+)         ; - + c
                  c)))
          ((char=? u #\.)
           (cond ((char=? v #\-)
                  (cond ((char=? c #\-)  ; . - -
                         #\+)
                        ((char=? c #\.)  ; . - .
                         #\-)
                        ((char=? c #\+)  ; . - +
                         #\.)))
                 ((char=? v #\.)         ; . . c
                  c)
                 ((char=? v #\+)
                  (cond ((char=? c #\-)  ; . + -
                         #\.)
                        ((char=? c #\.)  ; . + .
                         #\+)
                        ((char=? c #\+)  ; . + +
                         #\-)))))
          ((char=? u #\+)
           (cond ((char=? v #\-)         ; + - c
                  c)
                 ((char=? v #\.)
                  (cond ((char=? c #\-)  ; + . -
                         #\.)
                        ((char=? c #\.)  ; + . .
                         #\+)
                        ((char=? c #\+)  ; + . +
                         #\-)))
                 ((char=? v #\+)
                  (cond ((char=? c #\-)  ; + + -
                         #\+)
                        ((char=? c #\.)  ; + + .
                         #\-)
                        ((char=? c #\+)  ; + + +
                         #\.)))))
          )))
; Carry il riporto
(define btr-carry                    ; val:     carattere +/./-
  (lambda (u v c)                        ; u, v, c: caratteri +/./-
    (cond ((char=? u #\-)                ; u v c
           (cond ((char=? v #\-)         ;SECONDO VALORE = -
                  (cond ((char=? c #\-)  ; - - -
                         #\-)
                        ((char=? c #\.)  ; - - .
                         #\-)
                        ((char=? c #\+)  ; - - +
                         #\.)))
                 ((char=? v #\.)         ;SECONDO VALORE = .
                  (cond ((char=? c #\-)  ; - . -
                         #\-)
                        ((char=? c #\.)  ; - . .
                         #\.)
                        ((char=? c #\+)  ; - . +
                         #\.)))
                 ((char=? v #\+)         ; - + c
                  #\.)))
          ((char=? u #\.)                ;PRIMO CARATTERE = .
           (cond ((char=? v #\-)         ;SECONDO CARATTERE = - 
                  (cond ((char=? c #\-)  ; . - -
                         #\-)
                        ((char=? c #\.)  ; . - .
                         #\.)
                        ((char=? c #\+)  ; . - +
                         #\.)))
                 ((char=? v #\.)         ; . . c
                  #\.)
                 ((char=? v #\+)         ;SECONDO CARATTERE = +
                  (cond ((char=? c #\-)  ; . + -
                         #\.)
                        ((char=? c #\.)  ; . + .
                         #\.)
                        ((char=? c #\+)  ; . + +
                         #\+)))))
          ((char=? u #\+)                ;PRIMO CARATTERE = +
           (cond ((char=? v #\-)         ; + - c
                  #\.)
                 ((char=? v #\.)
                  (cond ((char=? c #\-)  ; + . -
                         #\.)
                        ((char=? c #\.)  ; + . .
                         #\.)
                        ((char=? c #\+)  ; + . +
                         #\+)))
                 ((char=? v #\+)       
                  (cond ((char=? c #\-)  ; + + -
                         #\.)
                        ((char=? c #\.)  ; + + .
                         #\+)
                        ((char=? c #\+)  ; + + +
                         #\+)))))
          )))  
(define btr-carry-sum1 ; mette tutti i calcoli insieme
  (lambda (s r c)
    (cond ((and (= (string-length r) 0) (> (string-length s) 0))  ;controllo per r=0 e s>0
           (string-append (btr-carry-sum1 (head s) "" (btr-carry (lsd s) #\. c))
                          (string(btr-digit-sum  (lsd s) #\. c)))
           )
          ((and (= (string-length s) 0) (> (string-length r) 0))  ;controllo per s=0 e r>0
           (string-append (btr-carry-sum1 "" (head r) (btr-carry #\. (lsd r) c))
                          (string (btr-digit-sum #\. (lsd r) c)))
           )
          ((and (= (string-length s) 0) (= (string-length r) 0))  ;controllo per s,r=0
           (string c))
          (else                                                   ; else = s>0, r>0
           (string-append (btr-carry-sum1 (head s) (head r) (btr-carry (lsd s) (lsd r) c))
                          (string (btr-digit-sum (lsd s) (lsd r) c)))
           )
          )
    )
  )
(define btr-carry-sum  ;FUNZIONE DA USARE
  (lambda (s r)
    (normalized-btr (btr-carry-sum1 s r #\.))
        )
    )