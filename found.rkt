(module found-wrapper mzscheme
  (define foundValue 'found)
  (define notfound 'notfound)

  
  (define (not-found x) (cons notfound x))
  (define (found x) (cons foundValue x))
  
  (define (found? x) (eq? (car x) foundValue))
  (define (found-value found) (cdr found))

  (provide not-found found found? found-value))