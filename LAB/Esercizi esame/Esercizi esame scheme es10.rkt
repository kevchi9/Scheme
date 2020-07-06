;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |BHO DIOCAN|) (read-case-sensitive #t) (teachpacks ((lib "drawings.rkt" "installed-teachpacks") (lib "hanoi.rkt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.rkt" "installed-teachpacks") (lib "hanoi.rkt" "installed-teachpacks")) #f)))
(define sorted-list2
  (lambda (n lst)
    (cond ((null? lst) (cons n lst)
                       )
          ((char=? n (first lst))
           lst)
          ((char<? n (first lst))
           (cons n lst)
           )
          (else (cons (first lst) (sorted-list2 n (rest lst))))
          )
    )
  )
(define sorted-list
  (lambda (lst)
    (if (string=? lst "")
        null
        (sorted-list2 (string-ref lst 0) (sorted-list(substring lst 1)))
        ))
    )
  (sorted-list "cba")