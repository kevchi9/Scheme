;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Laboratorio 4:11|) (read-case-sensitive #t) (teachpacks ((lib "drawings.rkt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.rkt" "installed-teachpacks")) #f)))
(define bit-val    ;val: 0 o 1
  (lambda (bit)    ; bit :carattere
    (if (char=? bit #\0) 0 1)
    ))
(define bin-dec-integer     ;val: intero
  (lambda (num)      ;num:stringa non vuota di 0/1
    (let ( (k (- (string-length num) 1)) )
    (let ( (pre (substring num 0 k)) (lsb (string-ref num k)) )
      (if (= k 0)
          (bit-val lsb)
          (+ (* 2 (bin-dec-integer pre)) )
          )))
    ))
(define integer-part
  (lambda (num)
    (cond ((string=? num "") "")
          ((char=? (string-ref num 0) #\.) "")
          (else (string-append
                 (substring num 0 1)
                               (integer-part (substring num 1))
                               )
                ))
    ))
(define fractional-part
  (lambda (num)
      (cond ((string=? num "") "")
          ((char=? (string-ref num 0) #\.) (substring num 1))
          (else (fractional-part (substring num 1))
                 ))
          ))
(define bin-dec-fract ;converte i numeri dopo la virgola
  (lambda (num)
    (let ((post (substring num 1)) (msb (string-ref num 0)) )
      (if (string=? post "")
          (/ (bit-val msb) 2)
          (+ (/ (bit-val msb) 2) (/ (bin-dec-fract post) 2))
          ))))

(define bin-dec-tot
  (lambda (num)
     (if (char=? (string-ref num 0) #\-)
         (cond ((string=? (fractional-part num) "")  ;caso in cui la parte frazionaria non è presente
                         (string-append "-" (bin-dec-integer (integer-part (substring num 1))))) 
                (else (string-append "-" (number->string (+ (bin-dec-integer (integer-part (substring num 1))) ;caso in cui ci siano sia intero che frazionario
                        (bin-dec-fract (fractional-part num)))))))
         (cond ((string=? num "")"")
               ((string=? (fractional-part num) "")  ;caso in cui la parte frazionaria non è presente
                          (bin-dec-integer (integer-part num)))
               (else (+ (bin-dec-integer (integer-part num)) ;caso in cui ci siano sia intero che frazionario
                        (bin-dec-fract (fractional-part num))))
    ))
    ))
  (define segno
  (lambda (num)
    (if (char=? (string-ref num 0) #\-)
        (substring num 1)
         num)))
        