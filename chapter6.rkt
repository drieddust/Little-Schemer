; Auxiliary functions
(define atom?
  (lambda (x)
    (not (list? x))))
;(numbered? expr) --> bool returns true if expr contains only numbers and +,*,/,-
; This function will also accept '(1 + 3 + (4 * 10))
(define numbered?
  (lambda (expr)
    (cond((atom? expr) (number? expr))
         ((eq? (car (cdr expr)) '+) (and (numbered? (car expr)) (numbered? (car (cdr (cdr expr))))))
         ((eq? (car (cdr expr)) '*) (and (numbered? (car expr)) (numbered? (car (cdr (cdr expr))))))
         ((eq? (car (cdr expr)) '/) (and (numbered? (car expr)) (numbered? (car (cdr (cdr expr))))))
         (else #f))))
;(numbered? '(1 + (3 + (4 mex 3))))

;simplified numbered? assumes we are providing only numbers are arthimetic symbols. If something else ( 1 foo 2) is given between numbers expression is still valid.
(define numbered?
  (lambda (expr)
    (cond((atom? expr)(number? expr))
         (else(and (numbered? (car expr)) (numbered? (car (cdr (cdr expr)))))))))
;(numbered? '(1 + (3 + (4 mex 3))))

;(value expr)--> return evaluated arthematic expression.
(define value
  (lambda (expr)
    (cond((and (atom? expr)(number? expr)) expr)
         ((eq? (car (cdr expr)) '+) (+ (value(car expr)) (value(car(cdr (cdr expr))))))
         ((eq? (car (cdr expr)) '*) (* (value(car expr)) (value(car(cdr (cdr expr))))))
         ((eq? (car (cdr expr)) '/) (/ (value(car expr)) (value(car(cdr (cdr expr))))))
         (else #f))))
;(value '(1 + (3 * (16 / 2))))

;value redefined to handle prefix representation of arthematix expression i.e. (value '(+ (* 2 3) (/ 15 5)))
(define value
  (lambda (expr)
    (cond((and (atom? expr) (number? expr)) expr)
         ((eq? (car expr) '+)(+ (value(car (cdr expr)))  (value(car(cdr (cdr expr))))))
         ((eq? (car expr) '*)(* (value(car (cdr expr)))  (value(car(cdr (cdr expr))))))
         ((eq? (car expr) '/)(/ (value(car (cdr expr)))  (value(car(cdr (cdr expr))))))
         (else #f))))
;(value '(+ 1 (* 3 (/ 15 4))))

;(1st-sub-exp expr) --> expr returns first sub expression assuming prefix representation

(define 1st-sub-exp
  (lambda (expr)
    (car (cdr expr))))

;(1st-sub-exp '(+ 1 (* 3 (/ 15 4))))

;(2nd-sub-exp expr)--> returns remaining sub expression of prefix representation of arthematic expression
(define 2nd-sub-exp
  (lambda (expr)
    (car (cdr (cdr expr)))))
;(2nd-sub-exp '(+ 1 (* 3 (/ 15 4))))

;(operator nexpr) --> extracts operator from the prefix notated arthematic expression
(define operator
  (lambda (expr)
    (car expr)))

;Another implementation of value using helper functions 1st-sub-exp and 2nd-sub-exp
; This implementation is representation agnostic provided we are ready to rewrite helpers operator,1st-sub-exp, and 2nd-subexp
(define value
  (lambda (expr)
    (cond((and (atom? expr) (number? expr)) expr)
         ((eq? (operator expr) '+)(+ (value(1st-sub-exp expr))  (value(2nd-sub-exp expr))))
         ((eq? (operator expr) '*)(* (value(1st-sub-exp expr))  (value(2nd-sub-exp expr))))
         ((eq? (operator expr) '/)(/ (value(1st-sub-exp expr))  (value(2nd-sub-exp expr))))
         (else #f))))
(value '(+ 1 (* 3 (/ 15 4))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Alternate representation of numbers using list so zero is repserented as '(), one as '(()())
;;;; We need to define primitive latematics function which are zero?,add1,sub1, and number?
;;;; Once these are redefined as per our list representation, all other fucntions will work smoothly
(define sero?
  (lambda (x)
    (eq? x '())))

(sero? '())

(define edd1
  (lambda (x)
    (cons '() x)))

(define zub1
  (lambda (x)
    (cdr x)))

;(sub1 '())

