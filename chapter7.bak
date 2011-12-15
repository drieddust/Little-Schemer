;; Auxiliary functions
(define atom?
  (lambda (x)
    (not (list? x))))

(define member?
  (lambda (a lat)
    (cond((null? lat)#f)
         ((eq? a (car lat)) #t)
         (else(member? a (cdr lat))))))

(define multirember
  (lambda (a lat)
    (cond((null? lat) '())
         ((eq? a (car lat)) (multirember a (cdr lat)))
         (else(cons (car lat) (multirember a (cdr lat)))))))

;(firsts lst) --> lst takes a lst composed of sublists and return list composed of first element of each internal sublist.
(define firsts
  (lambda (lst)
    (cond((null? lst) '())
         (else(cons (car (car lst)) (firsts (cdr lst)))))))
;; End of Auxiliary function

;;(set? lat)-->bool returns #t if list of atoms lat is a Set
(define set?
  (lambda (lat)
    (cond((null? lat)#t)
         ((member? (car lat) (cdr lat)) #f)
         (else(set? (cdr lat))))))
;;(set? '(a s s d))

;;(makeset lat)--> set. Removes duplicates from lat and returns set.
(define makeset
  (lambda (lat)
    (cond((null? lat) '())
         ((member? (car lat) (cdr lat))(makeset (cdr lat)))
         (else(cons (car lat) (makeset (cdr lat)))))))
;;(makeset '(a a s d s a w d r z))

;;makeset using multirember
(define makeset
  (lambda (lat)
    (cond((null? lat)'())
         (else(cons (car lat) (makeset (multirember (car lat) (cdr lat))))))))

;;(makeset '(a a s d s a w d r z))

;;(subset? set1 set2)--> bool returns true if set1 is a subset of set2
(define subset?
  (lambda (set1 set2)
    (cond((null? set1)#t)
         (else(and(member? (car set1) set2) (subset? (cdr set1) set2))))))
;;(subset? '(a) '())

;;(eqset? set1 set2) --> bool returns true if both sets are equal
(define eqset?
  (lambda (set1 set2)
    (cond((and(null? set1)(null? set2))#t)
         ((or(null? set1)(null? set2))#f)
         (else(and(eq? (car set1) (car set2)) (eqset? (cdr set1) (cdr set2)))))))
;;(eqset? '(a s) '(a s))

;;eqset? using subset?
(define eqset?
  (lambda (set1 set2)
    (and(subset? set1 set2) (subset? set2 set1))))
;;(eqset? '(a s) '(a s))


;;(intersect? set1 set2)-->bool
;; Returns true of atleast one element is common.
(define intersect?
  (lambda (set1 set2)
    (cond((null? set1)#f)
         (else(or(member? (car set1) set2)(intersect? (cdr set1) set2))))))

;;(intersect? '() '())

;;(intersect set1 set2)--> set
;; returns common elements of set1 and set2
(define intersect 
  (lambda (set1 set2)
    (cond((null? set1) '())
         ((member? (car set1) set2)(cons (car set1)(intersect (cdr set1) set2)))
         (else(intersect (cdr set1) set2)))))

;;(intersect '(a s d) '(s w d))

;;(union set1 set2) --> set
(define union
  (lambda (set1 set2)
    (cond((null? set1) set2)
         ((null? set2) set1)
         ((member? (car set1) set2)(union (cdr set1) set2))
         (else(cons(car set1) (union (cdr set1) set2))))))

;;(union '(a f s) '(d f s))

;;(intersect-all l-set)--> lst
;; takes l-set which is a list of non empty sets  and returns a list which contains element present in all the sublists
(define intersect-all
  (lambda (l-set)
    (cond((null? (cdr l-set))(car l-set))
         (else(intersect (car l-set) (intersect-all (cdr l-set)))))))

;;(intersect-all '((a s d) (a d e)))

;; (a-pair? l)--> bool
;;  returns true if l is a pair of anytype.
(define a-pair?
  (lambda (l)
    (cond((atom? l)#f)
         ((null? l) #f)
         ((null? (cdr l)) #f)
         ((eq? (cdr (cdr l)) '())#t)
         (else #f))))
;(a-pair? '(a s))

;; (first s-expr) --> s-expr 
;;  Return first sub-expression
(define first
  (lambda (s-expr)
    (cond((atom? s-expr)#f)
         ((null? s-expr) #f)
         (else(car s-expr)))))
;;(first '(a s d))

;; (second s-expr) --> s-expr
;; return second s-expr
(define second
  (lambda (s-expr)
    (cond((atom? s-expr)#f)
         ((null? s-expr)#f)
         ((null?(cdr s-expr))#f)
         (else (car (cdr s-expr))))))
;;(second '(a s d))

;;(build s1 s2)---> lst
;; return a list lst by consing s1 with s2

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))
;;(build '1 '(2 3 4))

;;(third  lst) --> s-expr
;; Return third expression fron the list lst
(define third
  (lambda (lst)
    (cond((atom? lst)#f)
         ((null? lst)#f)
         ((null?(cdr lst))#f)
      (else( car (cdr (cdr lst)))))))
;;(third '(1 2 3))

;;(fun? rel) --> bool
;; returns true if relation rel is a function.
(define fun?
  (lambda (rel)
    (set? (firsts rel))))

;;(fun? '((1 2 1) (1 4)))

;; (revrel rel)--> reversed-rel
;; takes relation rel which is a list of pairs and returns reversed-rel in which items within par are reversed.

(define revrel
  (lambda (rel)
    (cond((null? rel)'())
         ((null? (third (car rel))) (cons (build (car(cdr(car rel))) (car(car rel))) (revrel (cdr rel))))
         (else'()))))

;;(revrel '((1 2) (3 4)))

