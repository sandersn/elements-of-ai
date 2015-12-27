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
[repl-eval "(match2 '(a (b) c) '(x y z))\n"]
(match3 '(a b ? d) '(a b e d))