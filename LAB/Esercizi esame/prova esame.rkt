;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |prova esame|) (read-case-sensitive #t) (teachpacks ((lib "drawings.rkt" "installed-teachpacks") (lib "hanoi.rkt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.rkt" "installed-teachpacks") (lib "hanoi.rkt" "installed-teachpacks")) #f)))
(define first-half
  (lambda (s)
    (if (odd? (string-length s))
        (substring s 0 (+ (/ (string-length s) 2) 0.5)) 
        (substring s 0 (/ (string-length s) 2))
        )
    )
  )
(define second-half
  (lambda (s)
    (if (odd? (string-length s))
        (substring s (+ (/ (string-length s) 2) 0.5) (string-length s))
        (substring s (/ (string-length s) 2) (string-length s))
        )
    )
  )
(define palindrome?
  (lambda (s)
    (let ((first-l (substring (first-half s) 0 1)))
      (let (( n (string-length s)))
        (if (or (= n 0) (= n 1))
            true
            (let ((last-l (substring (second-half s) (- (string-length (second-half s)) 1)
                                     (string-length (second-half s)))))
              (if (string=? first-l last-l)
                  (palindrome? (substring s 1 (- (string-length s) 1)))
                  false)
              )
            )
        ))
    )
  )
(define lev-palindrome
  (lambda (s)
    (let ((first-l (substring (first-half s) 0 1)))
      (let (( n (string-length s)))
        (if (or (= n 0) (= n 1))
            (+ 1)
            (let ((last-l (substring (second-half s) (- (string-length (second-half s)) 1)
                                     (string-length (second-half s)))))
              (if (string=? first-l last-l)
                  (+ 1 (lev-palindrome (substring s 1 (- (string-length s) 1))))
                  (lev-palindrome (substring s 1 (- (string-length s) 1))))
              )
            )
        ))
    )
  )
;----------------------------------------------
(define xlcs ; val: stringa
  (lambda (s t) ; s, t: stringhe
    (cond ((string=? s "") t)
          ((string=? t "") 
           (string-append "/" (xlcs (substring s 1) t)))
          ((char=? (string-ref s 0) (string-ref t 0))
           (string-append "*" (xlcs (substring s 1) (substring t 1))))
          (else
           (better (string-append "/" (xlcs (substring s 1) t))
                   (string-append (substring t 0 1) (xlcs s (substring t 1)))
                   ))
          )))
(define better
  (lambda (u v)
    (if (< (stars u) (stars v))
        v
        u
        )))
(define stars
  (lambda (q)
    (if (string=? q "")
        0
        (let ((n (stars (substring q 1))))
          (if (char=? (string-ref q 0) #\*)
              (+ n 1)
              n)
          ))))
;--------------------------------------------------------
(define f ; val: intero
  (lambda (x y) ; x ≥ 0, y > 0 interi
    (if (< x y)
        1
        (+ (f (- x 1) y) (f (- x y) y))
        )))