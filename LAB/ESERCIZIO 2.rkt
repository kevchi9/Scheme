;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |ESERCIZIO 2|) (read-case-sensitive #t) (teachpacks ((lib "drawings.rkt" "installed-teachpacks") (lib "hanoi.rkt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.rkt" "installed-teachpacks") (lib "hanoi.rkt" "installed-teachpacks")) #f)))
(define s smaller-tile)
(define l larger-tile)

(define cross
  (lambda (st lt)
    (let ((1t ( shift-right st 1.7)))
      (let (( bt (glue-tiles lt (shift-down(shift-right (half-turn lt) 1.7) 0.7 ))))
        (let ((2t (shift-down(shift-right(half-turn st) 1.6) 3.9) ))
          (glue-tiles (glue-tiles 1t bt) 2t)
          )
        )
      )
    )
    )

(define square
  (lambda (st lt)
   (glue-tiles
    (glue-tiles
     (glue-tiles
      (shift-down (shift-right lt 1.6)0.8) (half-turn lt))
     (shift-right (half-turn st) 1.6))
    (shift-down(shift-right st 1.6) 4))
    )
  )
    
  