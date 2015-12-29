;; this is not the usual definition of atom?
;; but it's the one that Elements of AI seems to use.
;; the usual definition also checks (not (null? x))
(define (atom? x)
  (not (pair? x)))
(define (acons k v alist)
  (cons (cons k v) alist))
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
     (set! matches (acons (cadar p) (car s) matches))
     #t)
    (else #f)))
(define (match5 p s)
  (cond
    ((null? p) (null? s))
    ((or (atom? p) (atom? s)) #f)
    ((equal? (car p) (car s))
     (match5 (cdr p) (cdr s)))
    ((and
       (pair? (car p))
       (= (length (car p)) 2)
       (eq? (caar p) '?)
       (match5 (cdr p) (cdr s)))
     (set! matches (acons (cadar p) (car s) matches))
     #t)
    ((and
       (pair? (car p))
       (= (length (car p)) 2)
       (not (eq? (caar p) '?))
       (apply (eval (caar p)) (list (car s)))
       (match5 (cdr p) (cdr s)))
     (set! matches (acons (cadar p) (car s) matches))
     #t)
    (else #f)))

(define (match p s)
  (define matches '())
  (define (match^ p s)
    (cond
      ((null? p) (null? s))
      ; from here on, p is not null so it must be a cons
      ((atom? (car p))
       (and (not (atom? s))
            (equal? (car p) (car s))
            (match^ (cdr p) (cdr s))))
      ((and (not (null? s)) (eq? (caar p) '?))
       (cond 
         ((match^ (cdr p) (cdr s))
          (set! matches (acons (cadar p) (car s) matches))
          #t)
         (else #f)))
      ((eq? (caar p) '*)
       (cond
         ((and (not (null? s)) (match^ (cdr p) (cdr s)))
          (set! matches (acons (cadar p) (list (car s)) matches))
          #t)
         ((match^ (cdr p) s)
          (set! matches (acons (cadar p) '() matches))
          #t)
         ((and (not (null? s)) (match^ p (cdr s)))
          (let ((result (assq (cadar p) matches)))
            (set-cdr! result (cons (car s) (cdr result))))
          #t)
         (else #f)))
       ((and 
          (not (null? s))
          (apply (eval (caar p)) (list (car s)))
          (match^ (cdr p) (cdr s)))
        (set! matches (acons (cadar p) (car s) matches))
        #t)
       (else #f)))
  (if (match^ p s) matches #f))
 
(repl-eval "(match 
              '((* x) wild (? y) (symbol? z)) 
              '(* specifies a wild card sequence element))")
(match3 '(a b ? d) '(a b e d))
