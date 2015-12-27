;; this is not the usual definition of atom?
;; but it's the one that Elements of AI seems to use.
;; the usual definition also checks (not (null? x))
(define (atom? x)
  (not (pair? x)))
; structural equality
(define match1 equal?)
; structural isomorphism
(define (match2 p s)
  (cond
    ((atom? p) (atom? s))
    ((atom? s) #f)
    ((match2 (car p) (car s))
     (match2 (cdr p) (cdr s)))
    (else #f)))    
(define (match3 p s)
  (cond
    ((null? p) (null? s))
    ((or (atom? p) (atom? s)) #f)
    ((equal? (car p) (car s))
     (match3 (cdr p) (cdr s)))
    ((eq? (car p) '?) (match3 (cdr p) (cdr s)))
    (else #f)))
;; Scheme doesn't have bare `set`, so match4
;; adds to a global a-list instead.
;; This is *also* a terrible idea, but slightly better
;; than using `set`.
;; A Javascript version would, of course, set properties on `this`
;; without checking what `this` is bound to.
(define matches '())
(define (match4 p s)
  (cond
    ((null? p) (null? s))
    ((or (atom? p) (atom? s)) #f)
    ((equal? (car p) (car s))
     (match4 (cdr p) (cdr s)))
    ((and
       (pair? (car p))
       (= (length (car p)) 2)
       (eq? (caar p) '?)
       (match4 (cdr p) (cdr s)))
     (set! matches (cons (cons (cadar p) (car s)) matches))
     #t)
    (else #f)))
  
[repl-eval "(match4 '(x (? b) z) '(x y z))\n"]
(match3 '(a b ? d) '(a b e d))
