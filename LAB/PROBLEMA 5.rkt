;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |PROBLEMA 5|) (read-case-sensitive #t) (teachpacks ((lib "drawings.rkt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.rkt" "installed-teachpacks")) #f)))
(define manhattan ; val : intero numero di soluzioni
  (lambda (x y)  ; i distanza verticale, j distanza orizzontale
    (if (or (= x 0) (= y 0))
        1
        (+ (manhattan(- x 1) y) (manhattan x (- y 1)))
        )
    )
  )

(define manhattan-3D ; come manhattan ma con 3 dimensioni, diviso in 3 parti, in ciascuna una dimensione è settata a 0
  (lambda (x y z)
    (if (or (and (= x 0) (= y 0) (= z 0)) (and (= x 0) (= y 0)) (and (= x 0) (= z 0)) (and (= y 0) (= z 0)))
        1
        (cond ((= x 0) (+ (manhattan (- y 1) z) (manhattan y (- z 1))))
              ((= y 0) (+ (manhattan (- x 1) z) (manhattan x (- z 1))))
              ((= z 0) (+ (manhattan (- x 1) y) (manhattan x (- y 1))))
              (else (+ (manhattan-3D (- x 1) y z) (manhattan-3D x (- y 1) z) (manhattan-3D x y (- z 1))))
              )
    ))
  )
          