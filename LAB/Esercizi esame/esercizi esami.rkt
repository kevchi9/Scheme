;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |esercizi esami|) (read-case-sensitive #t) (teachpacks ((lib "drawings.rkt" "installed-teachpacks") (lib "hanoi.rkt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.rkt" "installed-teachpacks") (lib "hanoi.rkt" "installed-teachpacks")) #f)))
(define convert
  (lambda (n)
    (if (= n 0)
        null
        (if (= n 1)
            (list (remainder  n 2))
            (if (odd? n)
                (append  (list(remainder  n 2)) (convert (- (/ n 2) 0.5)))
                (append (list(remainder n 2)) (convert (/ n 2)))
                )
            )
        )
    ))
(define calc
  (lambda (ls k)
    (if (= (length ls) 0)
        null
        (if (= (length ls) 1)
            (if (= (car ls) 0)
                null
                (list (* (car ls) (expt 2 k))))
            (if (= (car ls) 0)
                (calc (cdr ls) (+ k 1))
                (append (list (* (car ls) (expt 2 k))) (calc (cdr ls) (+ k 1)))
                ))
            )
        )
    )
(define powers-of-two
  (lambda (n)
    (reverse (calc (convert n) 0)
    )
  )
  )
  
