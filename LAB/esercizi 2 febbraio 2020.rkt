;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |esercizi 2 febbraio 2020|) (read-case-sensitive #t) (teachpacks ((lib "drawings.rkt" "installed-teachpacks") (lib "hanoi.rkt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.rkt" "installed-teachpacks") (lib "hanoi.rkt" "installed-teachpacks")) #f)))
(define xtassellations
  (lambda (n c);  n=lunghezza , k=tasselli da due posti , c=controllo per due pezzi da due allineati
    (if (= n 0)
        1
        (if (> n 1)
            (if (= c 1)
                (+ (xtassellations (- n 1) 0))
                (+ (xtassellations (- n 2) 1) (xtassellations (- n 1) 0))
                )
            (+ (xtassellations (- n 1) 0))
            )
        )
    )
  )
(define fill
  (lambda (n)
    (xtassellations n 0))
  )
;----------------------

(define f
  (lambda (x y)
    (g x y 0) ))
(define g
  (lambda (x y k)
    (cond ((or (= (string-length x) k) (= (string-length y) k)) (if (> k 0) (list (substring x 0 k)) null))
          ((char=? (string-ref x k) (string-ref y k)) (g x y (+ k 1)))
          ((> k 0)
           (cons (substring x 0 k) (g (substring x k) (substring y k) 0)))
          (else
           (let ((u (g (substring x 1) y 0)) (v (g x (substring y 1) 0)))
             (if (< (h u) (h v)) v u)))
          )))
(define h
  (lambda (u)
    (if (null? u)
        0
        (+ (string-length (car u)) (h (cdr u))) )))