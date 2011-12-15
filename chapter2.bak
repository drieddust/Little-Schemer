#lang racket

(define atom?
  (lambda (x)
    (not (list? x))))

;Define lat? which returns true if a list of atoms is given.
(define lat?
  (lambda (l)
    (cond((null? l) #t)
         ((atom? (car l))(lat? (cdr l)))
         (else #f))))
(lat? '(a s d))

; (member? s l) --> #t if symbol s is in list l
(define member?
  (lambda (s l)
    (cond ((null? l) #f)
          ((or (eq?  (car l) s) (member? s (cdr l))))
          (else #f))))
(member? 'a '(w q (a) s))
           