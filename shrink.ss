(define (atom? x)
  (not (pair? x)))
(define (acons k v alist)
  (cons (cons k v) alist))
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
(define (printl l)
  (cond ((null? l) (display "\n"))
        (else 
          (display (car l))
          (display " ")
          (printl (cdr l)))))
(define (shrink)
  (define wword-count 0)
  (define punt-count 0)
  [display "WELCOME TO MY SOFA!\n"]
  [display "PLEASE DO NOT USE ALL CAPS TO TALK.\n"]
  [define rules
    `[[[you are [* x]] . ,[lambda [x] `[please tell me ,[wword] you are ,x]]]
      [[you feel [* x]] . ,[lambda [x] '[i sometimes feel the same way]]]
      [[you have [* x]] . ,[lambda [x] `[how long have you had ,x]]]
      [[because [* x]] . ,[lambda [x] `[is that really the reason]]]
      [[] . ,[lambda [x] '[please say something]]]
      [[yes [* x] . ,[lambda [x] `[how can you be so sure ,x]]]
     ]]
  [define [loop]
    [let [[s [you-me-map [read]]]]]
      [cond
        [[match '[bye] s] 'goodbye]
        [[match '[you are '[* x]] s]
        [else '???]]]]
  [loop]
  (display "not started yet really\n"))