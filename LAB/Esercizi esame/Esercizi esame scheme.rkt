;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Esercizi esame scheme|) (read-case-sensitive #t) (teachpacks ((lib "drawings.rkt" "installed-teachpacks") (lib "hanoi.rkt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.rkt" "installed-teachpacks") (lib "hanoi.rkt" "installed-teachpacks")) #f)))
(define match
 (lambda (u v)
   (if (or (string=? u "") (string=? v ""))
       ""
       (let ((uh (string-ref u 0)) (vh (string-ref v 0))
                                   (s (match (substring u 1)(substring v 1)))
              )
         (if (char=? uh vh)
             (string-append (string uh) s)
             (string-append "*" s)
             ))
       )))
(define offset (char->integer #\0)) ; 0 in ASCII

(define last-digit
  (lambda (base) (integer->char (+ (- base 1) offset)) )) ; dice qual'è il valore più grande della base
;per esempio in base due il valore più grande è 1, in 3 è 2...

(define next-digit
  (lambda (dgt) (string(integer->char (+ (char->integer dgt) 1))) )) 

(define increment
  (lambda (num base) ; 2 <= base <= 10
    (let ((digits (string-length num))) ;<- lunghezza stringa numerica
      (if (= digits 0) ;se la stringa è vuota (cioè vale 0) mi torna 1
          "1"
          (let ((dgt (string-ref num (- digits 1))))
            (if (char=? dgt (last-digit base))
                (string-append (increment (substring num 0 (- digits 1)) base)
                               "0")
                (string-append (substring num 0 (- digits 1)) (next-digit dgt))
                ))
          ))))
(define lcs ; valore: lista di terne
  (lambda (u v) ; u, v: stringhe
    (lcs-rec 1 u 1 v)
    ))

(define lcs-rec
  (lambda (i u j v)
    (cond ((or (string=? u "") (string=? v ""))
           '())
          ((char=? (string-ref u 0) (string-ref v 0))
           (cons  (list i j (substring u 0 1))
            (lcs-rec (+ i 1) (substring u 1) (+ j 1) (substring v 1)) ))
          (else 
           (better (lcs-rec (+ i 1) (substring u 1) j v)
                   (lcs-rec i u (+ j 1) (substring v 1))
            ))
          )))

(define better
  (lambda (x y)
    (if (< (length x) (length y)) y x)
    )
  )
    