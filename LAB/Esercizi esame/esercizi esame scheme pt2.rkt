;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |esercizi esame scheme pt2|) (read-case-sensitive #t) (teachpacks ((lib "drawings.rkt" "installed-teachpacks") (lib "hanoi.rkt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.rkt" "installed-teachpacks") (lib "hanoi.rkt" "installed-teachpacks")) #f)))
(define cyclic-string
  (lambda (s n)
    (cond ((= (string-length s) n)
           s)
          ((< (string-length s) n)
           (string-append s (cyclic-string s (- n (string-length s)))))
          (else 
           (cyclic-string (substring s 0 (- (string-length s) 1)) n))
          )
    )
  )
(define av2
  (lambda (L)
    (if (= (length L) 2)
        (cond ((= (+ (list-ref L 0) (list-ref L 1)) 0)
               '(0))
              ((< (+ (list-ref L 0) (list-ref L 1)) 0)
               '(-1))
              (else 
               '(1))
    )
        (cond ((= (+ (list-ref L 0)
                 (list-ref L 1)) 0)
           (list* 0 (av (remove (list-ref L 0) L))))
          ((< (+ (list-ref L 0) (list-ref L 1)) 0)
           (list* -1 (av (remove (list-ref L 0) L))))
          (else 
           (list* 1 (av (remove (list-ref L 0) L)))))
    )
    )
  )
(define av
  (lambda (l)
    (av2 l)
    )
  )