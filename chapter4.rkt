#lang racket
; (add1 n) --> n+1 function takes a number and increases it by 1

(define add1
  (lambda (n)
    (+ n 1)))

;(add1 33)

;(sub1 n) --> n-1 function takes a number n and decreases by 1
(define sub1 
  (lambda (n)
    (- n 1)))
;(sub1 0)

;(new+ n1 n2) --> n1+n2 takes two argument and returns their sum.
(define new+
  (lambda (n1 n2)
    (cond((zero? n2) n1)
         (else(new+ (add1 n1) (sub1 n2))))))
         
;(+ -22 1)

;(new- n1 n2) --> n1-n2 returns difference
(define new-
  (lambda (n1 n2)
    (cond((zero? n2) n1)
         (else(new- (- n1 1) (- n2 1))))))
;(new- 22 21)

;(tup? tup) --> #t if tup is a list of numbers
(define tup?
  (lambda (tup)
    (cond((null? tup) #t)
         (else(and (number? (car tup)) (tup? (cdr tup)))))))
;(tup? '(1 2 3 3))
;(addtup tup) --> n returns sum of all the elements of tup
(define addtup
  (lambda (tup)
    (cond((null? tup) 0)
         (else(+ (car tup) (addtup (cdr tup)))))))
;(addtup '(1 0 3 3 123 -333))

;(X n1 n2)--> n return product of n1 and n2
(define X
  (lambda (n1 n2)
    (cond((zero? n2) 0)
         (else(+ n1 (X n1 (- n2 1))))))) 
;(X 3 14)

;(tup+ tup1 tup2) -->tup adds tup1 and tup2 elementwise
(define tup+
  (lambda (tup1 tup2)
    (cond((null? tup2) tup1)
         ((null? tup1) tup2)
         (else(cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))
;(tup+ '(1 2 3) '())

;(> n m) return #t if n is greater than m else #f
(define >
  (lambda (n m)
    (cond((zero? n) #f)
         ((zero? m) #t)
         (else(> (- n 1) (- m 1))))))
;(> 4 3)
;(< n m) return #t if n is less than m otherwise #f
(define <
  (lambda (n m)
    (cond((zero? m) #f)
         ((zero? n) #t)
         (else(< (sub1 n) (sub1 m))))))
;(< 3 3)

;(= n m) --> returns #t only if n is equal to m
(define =
  (lambda (n m)
    (cond((and (zero? n) (zero? m)) #t)
         ((zero? n) #f)
         ((zero? m) #f)
         (else (= (sub1 n) (sub1 m)))))) 
;(= 3 2)

;(expr n m) --> n raises n to mth power.
(define expr1
  (lambda (n m)
    (cond((zero? m) 1)
         (else(* n (expr1 n (sub1 m)))))))
;(expr1 4 3)

;(divide m n) --> q divide m by n and return the quotient
(define divide
  (lambda (m n)
    (cond((< m n) 0)
         (else(+ (divide (- m n) n) 1)))))
;(divide 15 4)

;(length lat) --> n returns number n which is the length of the list.
(define length
  (lambda (lat)
    (cond((null? lat) 0)
         (else(add1 (length (cdr lat)))))))
;(length '(1 2 ( 4 3) 4))

;(pick n lat)--> lat(n) returns nth element of lat
(define pick
  (lambda (n lat)
    (cond((null? lat) '())
         ((eq? n 1) (car lat))
         (else(pick (sub1 n) (cdr lat))))))
;(pick 4 '(7 2 4 5))

;(rempick n lat)--> lat return a new list lat after removing nth element
(define rempick
  (lambda (n lat)
    (cond((null? lat) '())
         ((eq? n 1) (cdr lat))
         (else(cons (car lat) (rempick (sub1 n) (cdr lat)))))))
;(rempick 3 '(3 2 56 4 39))

;(no-nums lat)--> lat returns a new list after removing all numbers
(define no-nums
  (lambda (lat)
    (cond((null? lat) '())
         ((number? (car lat)) (no-nums(cdr lat)))
         (else(cons(car lat) (no-nums (cdr lat)))))))
;(no-nums '(2 3 4 wer 23 qw 333))

;(all-nums lat) --> tup returns a tuple tup of all numbers
(define all-nums
  (lambda (lat)
    (cond((null? lat) '())
         ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
         (else(all-nums(cdr lat))))))
;(all-nums '(eweew 1 2 3 ewewew 3434343 wwqqw 5445454 wqwqwq))

;(equan? a1 a2) --> #t if a1 and a2 are of same type
(define equan?
  (lambda (a1 a2)
    (cond((eq? (list? a1) (list a2)) #t)
         ((eq? (number? a1) (number? a2)) #t)
         ((eq? (not(number? a1)) (not(number? a2))))
         (else #f))))

;(occur a lat) -->  n returns n as number of occourances of a in lat
(define occur
  (lambda (a lat)
    (cond((null? lat) 0)
         ((eq? a (car lat)) (add1 (occur a (cdr lat))))
         (else(occur a (cdr lat))))))
;(occur 3 '(1 2 3 2 3 122121 3 32323232 5 3 323232 3))

;(one? n) -->bool return #t if n is one
(define one?
  (lambda (n)
    (= n 1)))
    
;(one? 1)
;(one? 2)

;(rempickusingone? n lat)--> lat returns lat after removing nth element
(define rempickusingone?
  (lambda (n lat)
    (cond((null? lat) '())
         ((one? n) (cdr lat))
         (else(cons (car lat) (rempickusingone? (- n 1) (cdr lat)))))))
(rempickusingone? 3 '(1 2 3 4 5 6 7))
         