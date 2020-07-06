;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname AS) (read-case-sensitive #t) (teachpacks ((lib "drawings.rkt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.rkt" "installed-teachpacks")) #f)))
(define belong
  (lambda (n l)
    (if (null? l)
        false
        (if (= (car l) n)
            true
            (belong n (cdr l)
                )
            )
        )
    )
  )
(define position
  (lambda (n l)
    (if (= (car l) n)
            0
            (+ 1 (position n (cdr l)
                ))
            )
    )
  )
(define sorted-ins
  (lambda (n l)
    (if (null? l)
        (cons n l)
        (cond ((null? n) l)
              ((= n (car l)) l)
              ((< (car (list n) ) (car l))
               (cons (car (list n)) (sorted-ins (cdr (list n)) l)
                     )
               )
              (else (cons (car l) (sorted-ins n (cdr l))))
          )
    )
  ))
(define sorted-list
  (lambda (l)
    (if (null? l)
        '()
        (sorted-ins (car l) (sorted-list (cdr l)))
    )
  )
)