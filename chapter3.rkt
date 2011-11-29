#lang racket
; Auxiliary functions
(define atom?
  (lambda (x)
    (not (list? x))))
;(rmember a lat) --> lat removes first occourance of symbol a from list of symbols lat

(define rmember
  (lambda (a lat)
    (cond((null? lat) '())
         ((eq? (car lat) a) (cdr lat))
         (else(cons (car lat) (rmember a (cdr lat)))))))
;(rmember 'a '(s w  a e a))

;(firsts lst) --> lst takes a lst composed of sublists and return list composed of first element of each internal sublist.
(define firsts
  (lambda (lst)
    (cond((null? lst) '())
         (else(cons (car (car lst)) (firsts (cdr lst)))))))
         
         
;(firsts '( (q w) (a) (d e)))

;(seconds lst) --> lst takes a list composed of sublists and returns a list composed of second elements of each internal sublist.
(define seconds
  (lambda (lst)
    (cond((null? lst) '())
         (else(cons (car (cdr (car lst))) (seconds (cdr lst)))))))
;(seconds '( (q w) (a a) (d e)))

;(insertR new old lat) --> lat takes 2 symbols new,old and a list lat of symbols and returns a new list with new inserted to the right of old.
(define insertR
  (lambda (new old lat)
    (cond((null? lat) '())
         ((eq? old (car lat)) (cons old (cons new (cdr lat))))
         (else(cons (car lat) (insertR new old (cdr lat)))))))
;(insertR 2 4 '(1 3 4 5 7 3 4))

;(insertL new old lat) --> lat takes 2 symbols new,old and a list lat of symbols and returns a new list with new inserted to the left of old.
(define insertL
  (lambda (new old lat)
    (cond((null? lat) '())
         ((eq? old (car lat)) (cons new (cons old (cdr lat))))
         (else(cons(car lat) (insertL new old (cdr lat)))))))
;(insertL 2 4 '(1 3 4 5 7 3 4))

;(subst new old lat) --> lat takes 2 symbols new,old and a list lat of symbols and return a new list with old substituted with new.
(define subst
  (lambda (new old lat)
    (cond((null? lat) '())
         ((eq? old (car lat)) (cons new (cdr lat)))
         (else(cons(car lat) (subst new old (cdr lat)))))))
;(subst 2 3 '(1 2 3 4 5 2 3 10))

;(subst2 new ol1 ol2 lat) --> lat takes 3 symbols new,ol1,ol2 and a list lat of symbols and return a new list with either ol1 or ol2  substituted with new.
(define subst2
  (lambda (new ol1 ol2 lat)
    (cond((null? lat) '())
         ((or (eq? ol1 (car lat)) (eq? ol2 (car lat))) (cons new (cdr lat)))
         (else(cons(car lat) (subst2 new ol1 ol2 (cdr lat)))))))
;(subst2 2 3  5 '(1 2 5 4 5 2 3 6 8))

;(multirember a lat) --> lat takes a symbol a and a list lat and returns a list with all elements a removed.
(define multirember
  (lambda (a lat)
    (cond((null? lat) '())
         ((eq? a (car lat)) (multirember a (cdr lat)))
         (else (cons (car lat) (multirember a (cdr lat)))))))
;(multirember 2 '(1 2 3 2 4 2 6))

;(multiinsertR new old lat) --> lat takes 2 symbols new,old and a list lat and return a list with new inserted to right of all occourances of old.
(define multiinsertR
  (lambda (new old lat)
    (cond((null? lat) '())
         ((eq? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat)))))
         (else(cons (car lat) (multiinsertR new old (cdr lat)))))))
;(multiinsertR  '10 '3 '(1 2 3 4 5 3 4))

;(multiinsertL new old lat) --> lat takes 2 symbols new,old and a list lat and return a list with new inserted to right of all occourances of old.
(define multiinsertL
  (lambda (new old lat)
    (cond((null? lat) '())
         ((eq? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat)))))
         (else(cons (car lat) (multiinsertL new old (cdr lat)))))))
;(multiinsertL  '10 '3 '(1 2 3 4 5 3 4))

;(multisubst new old lat) --> lat takes 2 symbols new,old and a list lat and returns a new list with all instances of old replaced with new.
(define multisubst
  (lambda (new old lat)
    (cond((null? lat) '())
         ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
         (else (cons(car lat) (multisubst new old (cdr lat)))))))
(multisubst '3 '2 '(1 2 2 1 22 3 4 2 5 2 32232 2))


