;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Esercitazione esame|) (read-case-sensitive #t) (teachpacks ((lib "drawings.rkt" "installed-teachpacks") (lib "hanoi.rkt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.rkt" "installed-teachpacks") (lib "hanoi.rkt" "installed-teachpacks")) #f)))
(define xtassellations
  (lambda (n k)
    (if (or (and (= n 0) (>= k 0)) (and (>= n 0) (= k 0)) (and (= n 0) (= k 0)))
        1
        (if (or (< n 0) (< k 0))
                0
                (+ (xtassellations (- n 2) (- k 1)) (xtassellations (- n 1) k))
                ))
            )
        )


(define vigenere-cipher
  (lambda (key)
    (lambda (msg)
      (if (string=? msg "")
          ""
          (let ((r (- (char->integer (string-ref key 0)) cA))
                (c (- (char->integer (string-ref msg 0)) cA))
                (k (string-append (substring key 1) (substring key 0 1)))
                )
            (string-append
             (string (integer->char (+ cA (if (> (+ cA r c) 90) (- (+ r c) 26) (+ r c)
                                              ))))
             ((vigenere-cipher k) (substring msg 1)) 
             ))
          )
      )))
(define cA (char->integer #\A))
(define encrypt (vigenere-cipher "AKEY"))
(define sorted-ins
  (lambda (w s)
    (if (string<? w (car s))
        (list* w s)
        (list* (car s) (sorted-ins w (cdr s)))
        )
    )
  )
(define permutation-rule
  (lambda (plain encrypted)
    (lambda (x) (permutation-rec x 0 plain encrypted))
    ))
(define permutation-rec
  (lambda (x i plain encrypted)
    (if (char=? x (string-ref plain i))
        (string-ref encrypted i)
        (permutation-rec x (+ i 1) plain encrypted)
        )))
(define pr (permutation-rule "ABCDEFGHILMNOPQRSTVX" "DEFGHILMNOPQRSTVXABC"))

(define gsum
  (lambda (a n)
    (cond ((= n 0) 0)
          ((= n 1) a)
           (else
            (+ a (* a (gsum a (- n 1))))
           ))))
