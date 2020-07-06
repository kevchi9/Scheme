;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |ESERCIZIO 3|) (read-case-sensitive #t) (teachpacks ((lib "drawings.rkt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.rkt" "installed-teachpacks")) #f)))
;---------------------------------------------------------------------PARTE 1-----------------------------------------------------------------------------------------
(define integer ; divide la parte intera dalla parte fratta
  (lambda (s)
    (if (= (string-length s) 0)
        ""
        (if (or (char=? (string-ref s 0) #\-) (char=? (string-ref s 0) #\+)) ; se il primo carattere è un segno, lo mette da parte
            (integer (substring s 1))
            (if (char=? (string-ref s 0) #\.) ; se il primo carattere è un punto, la parte intera non c'è o è stata già superata
                ""
                (string-append (string(string-ref s 0)) (integer (substring s 1)))
                ))
        )
        )
    )
(define fract-string ; divide la parte fratta da quella fratta 
  (lambda (s)
    (if (= (string-length s) 0) ; stesso funzionamento di intgeger
        ""
        (if (or (char=? (string-ref s 0) #\-) (char=? (string-ref s 0) #\+))
            (fract-string (substring s 1))
            (if (= (string-length s) 0)
                ""
                (if (char=? (string-ref s 0) #\.)
                    (substring s 1)
                    (fract-string (substring s 1))
                    )))
        )
    ))
(define b-r-n1
  (lambda (s)
    (b-r-n (integer s))
    ))
(define b-r-n ; calcola la parte intera da bin a dec
  (lambda (s)
    (if (char=? (string-ref s 0) #\.) ; se si trova il punto ci si ferma subito 
        0
        (if (= (string-length s) 1)
            (if (char=? (string-ref s (- (string-length s)1)) #\1) ; caso in cui rimane solo un numero da convertire
                (+ 1)
                0)
            (if (char=? (string-ref s (- (string-length s)1)) #\1) ; caso in cui rimangono più di un numero da convertire
                (+ (* 2 (b-r-n (substring s 0 (- (string-length s ) 1)))) 1)
                (* 2 (b-r-n (substring s 0 (- (string-length s ) 1))))
                )))
  ))
(define calculate-fract ; calcola la parte fratta 
  (lambda (s)
    (if (= (string-length s) 0) ; caso in cui non ci sono o non ci sopo più bit da convertire
        0
        (if (= (string-length s) 1) ; caso in cui manca solo un bit da convertire
            (if (char=? (string-ref s 0) #\1) ; se il bit è 1 divido per 2,altrimenti è 0.
                (/ 2)
                0)
            (if (char=? (string-ref s 0) #\1) ; caso in cui mancano più di un bit da convertire
                (+ (/ (calculate-fract (substring s 1)) 2) (/ 1 2))
                (/ (calculate-fract (substring s 1)) 2)
                )
            )
        ))
    )
(define fract ; passa alla funziona calculate fract SOLO la parte fratta
  (lambda (s)
    (calculate-fract (fract-string s))
    ))

(define bin-rep->number0 ; mette insieme le due funzioni di calcolo per la parte intera e quella fratta
  (lambda (s)
    (+ (b-r-n1 s) (fract s))
    )
  )
(define bin-rep->number ; richiama la funzione principale ma tiene conto anche del segno --- FUNZIONE DA USARE
   (lambda (s)
    (if (char=? (string-ref s 0) #\-)
        (- (bin-rep->number0 s))
        (bin-rep->number0 s))
    )
  )
;------------------------------------------------------------------------------ PARTE 2 --------------------------------------------------------------------------------
(define base ;val: intero, lunghezza base restituisce la quantità di valori presenti nella base utilizzata (lunghezza della base)
  (lambda (b) ; b: stringa che rappresenta la base
    (string-length b)
    ))

(define value ; val:intero valore del carattere all'intermo della base, funziona solo con caratteri contenuti nella base, altrimenti torna un valore più grande della base
  (lambda (b v)
    (cond  ((= (string-length v) 0) 0)
           ((= (string-length b) 0) 0)
           ((string=? (substring b 0 1) v) (+ 0)) ; se v è uguale al valore più piccolo di base allora è 0;
           (else (+ (value (substring b 1) v) 1) )
                )
            )
        )
(define rep->number0 ;val:intero  esegue tutti i calcoli della parte intera
  (lambda (b s)
    (let ((s-len (string-length s)))
      (if (= (string-length s) 1)
          (if (char=? (string-ref s (- s-len 1)) (string-ref b 0)) ; se la cifra meno significativa di s è uguale allo 0 della base, torna 0
              0
              (+ (value b s )) ; altrimenti esegue dei calcoli utilizzando value per ottenere il valore effettivo della stringa
              )
          (if (char=? (string-ref s (- s-len 1)) (string-ref b 0))   ; se l'ultimo carattere è lo zero della base
              (* (base b) (rep->number0 b (substring s 0 (- s-len 1)))) ; eseguo la ricorsione moltiplicando per la lunghezza della base
              (+ (* (base b) (rep->number0 b (substring s 0 (- s-len 1)))) ; altrimenti fa la somma tra la ricorsione (come sopra)...
                 (value b (substring s (- s-len 1) s-len)))      ; ... ed il valore dell'ultimo carattere di s
              )
          )) 
    )
  )
(define integer-string ; separa la parte intera da segni e parte decimale
  (lambda (s)
    (if (= (string-length s) 0)
        ""
        (if (or (char=? (string-ref s 0) #\-) (char=? (string-ref s 0) #\+))
            (integer-string (substring s 1))
            (if (char=? (string-ref s 0) #\.)
                ""
                (string-append (substring s 0 1) (integer-string (substring s 1))))
            )
            )
        )
    )
(define fract-string1 ; separa la parte decimale dalla parte intera
  (lambda (s)
    (if (= (string-length s) 0)
        ""
        (if (char=? (string-ref s 0) #\.)
            (substring s 1)
            (fract-string1 (substring s 1)))
        )
    )
  )
(define fract1 ; funziona solo con fract-string in input
  (lambda (b s)
    (let ((s-len (string-length s) ))
    (if (= s-len 0) ; caso in cui s è vuota
        0
        (if (= s-len 1) ; caso in cui s ha un carattere
            (if (char=? (string-ref s 0) (string-ref b 0)) ; caso in cui l'ultimo carattere di s è uno 0
                0
                (/ (value b (substring s 0 1)) (base b))) ; caso in cui non è uno 0
            (if (char=? (string-ref s 0) (string-ref b 0)) ; casi in cui s è > 1
                (/ (fract1 b (substring s 1)) (base b))
                (+ (/ (fract1 b (substring s 1)) (base b))
                (/ (value b (string-ref s 0)(base b)))
                )
            ))
        )
    )
  ))
(define rep->number ; val:intero numero codificato -- FUNZIONE UTENTE -- mette tutte le funzioni insieme ed esegue i calcoli
  (lambda (b s)
    (cond ((char=? (string-ref s 0) #\-)
           (- (+  (rep->number0 b (integer-string s)) (fract1 b (fract-string s)))))
          (else
           (+  (rep->number0 b (integer-string s)) (fract1 b (fract-string s))))
    )
  )
  )
(rep->number "0123456789ABDEF" "1CF.0")
