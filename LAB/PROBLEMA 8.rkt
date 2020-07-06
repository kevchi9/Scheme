;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |PROBLEMA 8|) (read-case-sensitive #t) (teachpacks ((lib "drawings.rkt" "installed-teachpacks") (lib "hanoi.rkt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.rkt" "installed-teachpacks") (lib "hanoi.rkt" "installed-teachpacks")) #f)))
(define hanoi-moves ; val: lista di coppie
  (lambda (n) ; n > 0 intero numero di dischi
    (hanoi-rec n 1 2 3)
    ))
(define hanoi-rec ; val: lista di coppie
  (lambda (n s d t) ; n intero, s, d, t: posizioni
    (if (= n 1)
        (list (list s d))
        (let ((m1 (hanoi-rec (- n 1) s t d))
              (m2 (hanoi-rec (- n 1) t d s))
              )
          (append m1 (cons (list s d) m2))
          ))
    ))
(define hanoi-disks1 ; costruisce le liste che indicano quanti dischi ci sono su ogni colonna
  (lambda (ls n k t1 t2 t3) ; k sono le mosse mancanti prima di fermarsi , ls è la lista delle mosse
    (if (or (= k 0) (null? ls)) ; se k = 0 o non ci sono mosse in ls
        (list (list 1 t1) (list 2 t2) (list 3 t3)) ; restituisce la lista dove t1,t2,t3 conterranno esattamente il numero dei dischi per chiascuna colonna
        (cond ((= (car(car ls)) 1) ; se il primo carattere della lista di coppie (lista delle mosse ls) è un uno vuol dire che il primo disco viene dalla prima colonna
              (if (= (cadar ls) 2) ; se il cadara è 2 vuol dire che il disco va tolto dalla prima colonna
                  (hanoi-disks1 (cdr ls) n (- k 1) (- t1 1) (+ t2 1) t3) ; e viene messo sulla seconda
                  (hanoi-disks1 (cdr ls) n (- k 1) (- t1 1) t2 (+ t3 1)) ; altrimenti viene messo sulla terza
                  ))
              ((= (car(car ls)) 2) ; analogamente all'if precedente, il disco viene tolto dalla seconda colonna
               (if (= (cadar ls) 1)
                   (hanoi-disks1 (cdr ls) n (- k 1) (+ t1 1) (- t2 1) t3) ; e inserito nella prima 
                   (hanoi-disks1 (cdr ls) n (- k 1) t1 (- t2 1) (+ t3 1)) ; altrimenti nella terza
                   ))
              ((= (car(car ls)) 3) ; in questo caso invece il disco viene preso dalla terza colonna
               (if (= (cadar ls) 1)
                   (hanoi-disks1 (cdr ls) n (- k 1) (+ t1 1) t2 (- t3 1)) ; dato alla prima colonna
                   (hanoi-disks1 (cdr ls) n (- k 1) t1 (+ t2 1) (- t3 1)) ; dato alla seconda colonna
                   ))
              ))
    ))
(define hanoi-disks ; FUNZIONE UTENTE PER OTTENERE LA LISTA DI COLONNE CON IL NUMERO DEI DISCHI
  (lambda (n k)
    (hanoi-disks1 (hanoi-moves n) n k n 0 0)
    )
  )
(define hanoi-pictures1            ;RICORDA DI INVERTIRE IL RISULTATO ALTRIMENTI NON FUNZIONA, resituisce l'elaborazione grafica di hanoi-disks
  (lambda (ls k ls1 ls2 ls3)     
    (if (= k 0)                    ; mosse esaurite
        (list ls1 ls2 ls3)         ; se le mosse sono esaurite mi ritorna la lista delle liste dei dischi
        (cond ((= (car(car ls)) 1) ;1) casi in cui muovo dalla colonna 1...
              (if (= (cadar ls) 2) ; alla colonna 2...                                      1->2
                  (hanoi-pictures1 (cdr ls) (- k 1)
                                (remove (list-ref ls1 (-(length ls1)1)) ls1)
                                (append  ls2 (list(list-ref ls1 (-(length ls1)1))))
                                ls3)
                  (hanoi-pictures1 (cdr ls) (- k 1) ; oppure alla colonna 3               1->3
                                (remove (list-ref ls1 (-(length ls1)1)) ls1)
                                ls2 
                                (append ls3 (list(list-ref ls1 (-(length ls1)1))) )
                  )))
              ((= (car(car ls)) 2) ;2) casi in cui mi muovo dalla colonna 2
               (if (= (cadar ls) 1); alla colonna 1 ...                                     2->1
                   (hanoi-pictures1 (cdr ls) (- k 1)
                                 (append ls1 (list(list-ref ls2 (-(length ls2)1))))
                                 (remove (list-ref ls2 (-(length ls2)1)) ls2)
                                 ls3)
                   (hanoi-pictures1 (cdr ls) (- k 1) ; ... oppure alla colonna 3          2->3
                                 ls1
                                 (remove (list-ref ls2 (-(length ls2)1)) ls2)
                                 (append ls3 (list(list-ref ls2 (-(length ls2)1)) ))
                                 )
                   ))
              ((= (car(car ls)) 3) ;3) casi in cui mi muovo dalla colonna 3
               (if (= (cadar ls) 1); alla colonna 1...                                      3->1
                   (hanoi-pictures1 (cdr ls) (- k 1)
                                    (append ls1 (list(list-ref ls3 (-(length ls3)1)) ))
                                    ls2
                                    (remove (list-ref ls3 (-(length ls3)1)) ls3)
                                    )
                   (hanoi-pictures1 (cdr ls) (- k 1) ;... oppure alla colonna 2           3->2
                                 ls1
                                 (append ls2 (list(list-ref ls3 (-(length ls3)1))))
                                 (remove (list-ref ls3 (-(length ls3)1)) ls3)
                                 )
                   ))
              ))
    ))
;eseguo hanoi-pictures con valori iniziali ls = (hanoi-moves n)

(define hanoi-pictures 
 (lambda (n k)
   (hanoi-pictures1 (hanoi-moves n) k  (reverse (list1 n)) null null))
 )
(define list1 ; genera una lista in base a quanti dischi ho
  (lambda (n)
    (if(= n 0)
      '()
      (append(list1 (- n 1))(list n) 
    )
  )
    )
  )
(define tower-image    ; genera l'immagine della torre presa in considerazione
  (lambda (ls n k p t)
    (if (null? ls)
        (towers-background 3)
        (if (= (length ls) 1)
            (disk-image (car ls) n p t)
            (above (tower-image (cdr ls) n k p (+ t 1)) (disk-image (car ls) n p t)
                   )
            )
        )
  ))
(define first-tower ;genera l'immagine della prima colonna
  (lambda (n k)
    (tower-image (car (hanoi-pictures n k)) n k 1 0) 
    )
  )
(define second-tower ;genera l'immagine della seconda colonna
  (lambda (n k)
    (tower-image (car(cdr (hanoi-pictures n k))) n k 2 0)
    )
  )
(define third-tower ;genera l'immagine della terza colonna
  (lambda (n k)
    (tower-image (car(cdr(cdr (hanoi-pictures n k)))) n k 3 0)
    )
  )
(define hanoi-towers ; FUNZIONE UTENTE 
  (lambda (n k)
    (let ((tb-n (towers-background n)))
    (cond ((and (= (car(cdr(car(hanoi-disks n k)))) 0) (= (car(cdr(car(cdr(hanoi-disks n k))))) 0)) ;controllo per prima e seconda torre
           (above (third-tower n k) tb-n)
           )
          ((and (= (car(cdr(car(hanoi-disks n k)))) 0) (= (car(cdr(car(cdr(cdr(hanoi-disks n k)))))) 0)) ;controllo per prima e terza
           (above (second-tower n k)  tb-n)
           )
          ((and (= (car(cdr(car(cdr(hanoi-disks n k))))) 0) (= (car(cdr(car(cdr(cdr(hanoi-disks n k)))))) 0 )) ;controllo per secondo e terzo
           (above (first-tower n k)  tb-n)
           )
          ((= (car(cdr(car(hanoi-disks n k)))) 0)
           (above (above (second-tower n k) (third-tower n k))  tb-n) ; controllo solo per prima torre
           )
          ((= (car(cdr(car(cdr(hanoi-disks n k))))) 0)
           (above (above (first-tower n k) (third-tower n k)) tb-n)
           )
          ((= (car(cdr(car(cdr(cdr(hanoi-disks n k)))))) 0 )
           (above (above (first-tower n k) (second-tower n k))  tb-n)
           )
          (else
           (above (above (above (first-tower n k)(second-tower n k)) (third-tower n k)) tb-n)
           ))
    )
  )
  )