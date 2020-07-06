;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |esercizi esame pt3|) (read-case-sensitive #t) (teachpacks ((lib "drawings.rkt" "installed-teachpacks") (lib "hanoi.rkt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.rkt" "installed-teachpacks") (lib "hanoi.rkt" "installed-teachpacks")) #f)))
;-------------------------------------------- 6
(define r-val
  (lambda (s)
    (if (string=? s "")
        0
        (+ (/ (-(char->integer(string-ref (decimal s) 0)) 48) 2) (/ (r-val (substring (decimal s) 1)) 2))
        )
    )
  )

(define decimal2
  (lambda (s p)
    (if (= (string-length s) p)
        s
        (cond ((or (char=? (string-ref s p) #\1) (char=? (string-ref s p) #\0)) (decimal2 s (+ p 1)))
              (else (substring s (+ p 1)))
        )
    )
  ))
(define decimal
  (lambda (s)
    (decimal2 s 0))
  )
;------------------------------------------------ 7
(define shared
  (lambda (l1 l2)
    (if (and  (= (length l1) 0) (= (length l2) 0))
              '()
              (cond ((= (length l1) 1)
                     (cond ((< (car l1) (car l2))
                           (cons l1 l2))
                           ((= (car l1) (car l2))
                            l2)
                           (else 
                            (cons (car l2) (shared l1 (cdr l2)))
                            )
                           ))
                     ((= (length l2) 1)
                      (cond ((< (car l2) (car l1))
                             (cons l2 l1))
                            ((= (car l2) (car l1))
                             l1)
                            (else
                             (cons (car l1) (shared (cdr l1) l2)))))
                     ((< (car l1) (car l2))
                      (cons (car l1) (shared (cdr l1) l2))
                      )
                     ((> (car l1) (car l2))
                      (cons (car l2) (shared l1 (cdr l2)))
                      )
                     (else
                      (cons (car l1) (shared (cdr l1) (cdr l2))))
                     )
              )
    ))
;----------------------------------------- 8
(define pcfA
  (lambda (a)
    (if (= (string-length a) 0) 
        (+ 0)
        (if (string=? (substring a 0 1) "0")
            (+ 0 (pcfA (substring a 1)))
            (+ 1 (pcfA (substring a 1)))
            )
        )
    ))
(define parity-check-failures2
  (lambda (ls)
   (cond ((odd? (pcfA (car ls)))
           (append'(0) (parity-check-failures2 (cdr ls))))
          ((odd? (pcfA (car ls)))
           (append '(1) (parity-check-failures2 (cdr ls))))
          ((odd? (pcfA (car ls)))
           (append '(2) (parity-check-failures2 (cdr ls))))
          ((odd? (pcfA (car ls)))
           (append '(3) (parity-check-failures2 (cdr ls))))
          (else '())
          ))
  )
;---------------------------------------------- 9
(define find-minimal-difference
  (lambda (l)
    (if (= (length l) 2)
        (- (car(cdr l)) (car l))
        (min (- (car(cdr l)) (car l)) (find-minimal-difference (cdr l)))
        )
    )
  )
(define closest-pair
  (lambda (l)
    (if (= (- (car(cdr l)) (car l)) (find-minimal-difference l))
        (list* (car l) (list (car(cdr l))))
        (closest-pair (cdr l))
        )
    )
  )
;--------------------------------------------------- 10


(define sorted-list2
  (lambda (n l)
    (if (= (string-length l) 0)
        (list n)
        (cond ((char=? n (string-ref l 0)) l)
              ((char<? n (string-ref l 0))
               (cons n (sorted-list2 (string-ref l 0) (substring l 1)))
                     )
              (else (cons (string-ref l 0) (sorted-list2 n (substring l 1))))
          )
    )
  ))
(define sorted-list
  (lambda (s)
    (sorted-list2 (string-ref s 0) (substring s 1))
    )
  )
