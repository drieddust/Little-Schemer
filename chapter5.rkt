;#lang racket
; Auxiliary functions
(define atom?
  (lambda (x)
    (not (list? x))))
;(rember* a lst) --> lst returns a new list lst with all occourances of symbol a removed
(define rember*
  (lambda (a lst)
    (cond((null? lst) '())
         ((atom? (car lst))
          (cond((eq? a (car lst)) (rember* a (cdr lst)))
               (else(cons (car lst) (rember* a (cdr lst))))))
         (else(cons (rember* a (car lst)) (rember* a (cdr lst)))))))
;(rember* 3 '(1 () (2 3 (4  ((3)) 5) 6) 3))


;(insertR* new old lst)--> lst returns a new list after inserting symbol new to the right of symbol old in the original lst.
(define insertR*
  (lambda (new old lst)
    (cond((null? lst) '())
         ((atom? (car lst))
          (cond((eq? (car lst) old) (cons old (cons new (insertR* new old (cdr lst)))))
               (else (cons (car lst) (insertR* new old (cdr lst))))))
          (else(cons(insertR* new old (car lst)) (insertR* new old (cdr lst)))))))
;(insertR* 4 3 '(1 () (2 3 (4  ((3)) 5) 6) 3))

;(insertL* new old lst) -- lst returns a new list after inserting symbol new to the left of symbols old in the original list lst.
(define insertL*
  (lambda (new old lst)
    (cond((null? lst) '())
         ((atom? (car lst))
          (cond((eq? old (car lst)) (cons new (cons old (insertL* new old (cdr lst)))))
               (else(cons (car lst) (insertL* new old (cdr lst))))))
         (else(cons(insertL* new old (car lst)) (insertL* new old (cdr lst)))))))
;(insertL* 4 3 '(1(2 3 (4((3)) 5) 6) 3))

;(occur* a lst) --> n returns number of occourances of symbol a in list lst
(define occur*
  (lambda (a lst)
    (cond((null? lst) 0)
         ((atom? (car lst))
          (cond((eq? a (car lst)) (add1 (occur* a (cdr lst))))
               (else(occur* a (cdr lst)))))
         (else(+(occur* a (car lst)) (occur* a (cdr lst)))))))

;(occur* 'banana '((banana) (split  ((((banana  ice))) (cream  (banana))sherbet)) (banana) (bread) (banana  brandy)))

;(subst* new old lst) --> lst returns a new list with all instances of old replaced with new
(define subst*
  (lambda (new old lst)
    (cond((null? lst) '())
         ((atom? (car lst))
          (cond((eq? old (car lst)) (cons new (subst* new old (cdr lst))))
               (else(cons (car lst) (subst* new old (cdr lst))))))
         (else(cons(subst* new old (car lst))(subst* new old (cdr lst)))))))
;(subst* 'orange 'banana '((banana)  (split  ((((banana  ice))) (cream  (banana)) sherbet)) (banana) (bread) (banana  brandy))) 

;(member* a lst) --> bool return #t if a exists anywhere inside the list lst
(define member*
  (lambda (a lst)
    (cond((null? lst) #f)
         ((atom? (car lst))
          (cond((eq? a (car lst)) #t)
               (else(member* a (cdr lst)))))
         (else(or(member* a (car lst))(member* a (cdr lst)))))))
;(member* 'chips '((potato)  (chips  ((with)  fish)  (chips))))

;(leftmost lst) --> a returns left symbol from list lst
(define leftmost
  (lambda (lst)
    (cond((null? lst) '())
         ((atom? (car lst)) (car lst))
         (else(or(leftmost (car lst)) (leftmost (cdr lst)))))))

;(leftmost '((potato)  (chips  ((with)  fish)  (chips))))
;(leftmost '(((()  four))  17  (seventeen)))

;(eqlist? l1 l2) --> bool return #t if both lists are equal
(define eqlist?
  (lambda (l1 l2)
    (cond((and (null? l1) (null? l2)) #t)
         ((or (null? l1) (null? l2)) #f)
         ((and (atom? (car l1)) (atom? (car l2))) (and (eq? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
       ;  (( or (atom? (car l1)) (atom? (car l2))) #f)
         (else(and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))
;(eqlist? '(beef  ((sausage))  (and  (soda))) '(beef  ((sausage))  (and  (soda))))

;(equal? a1 a2)--> bool returns #t if a1 and a2 of same type.
(define equal?
  (lambda (a1 a2)
    (cond ;((and(null? a1)(null? a2)) #t)
         ((and(atom? a1)(atom? a2))(eq? a1 a2))
         ((or(null? a1)(null? a2))#f)
         (else(eqlist? a1 a2)))))
;(equal? '1 '12)

; Redefine eqlist? using equal? only
;(eqlist? l1 l2)---> bool returns true if both lists are equal
(define eqlist?
  (lambda (l1 l2)
    (cond((and(null? l1)(null? l2)) #t)
         ((or (null? l1)(null? l2)) #f)
         (else(and(equal? (car l1)(car l2))(eqlist? (cdr l1)(cdr l2)))))))
;(eqlist? '(1 ((2)) 3) '(1 ((2)) 3))

;(rember a lst)--> lst returns lst after removing 

