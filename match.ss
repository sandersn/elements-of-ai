(define (atom? x)
  (not (or (pair? x) (null? x))))