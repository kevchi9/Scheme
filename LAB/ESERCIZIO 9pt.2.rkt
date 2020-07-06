;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |ESERCIZIO 9pt.2|) (read-case-sensitive #t) (teachpacks ((lib "drawings.rkt" "installed-teachpacks") (lib "hanoi.rkt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.rkt" "installed-teachpacks") (lib "hanoi.rkt" "installed-teachpacks")) #f)))
(define H
  (lambda (f g)
    (lambda (m n)
      (if (= n 0)
          (f m)
          (g m ((H f g) m (- n 1)))
          )
      )
    )
  )

(define s2
  (lambda (m n)
    (+ n 1)
    )
  )

(define add
  (H (lambda (i) i) s2)
  )

(define mul
  (H (lambda (z) 0) add)
  )

(define pow
  (H (lambda (u) 1) mul)
  )