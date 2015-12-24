(require mzlib/defmacro)
(require scheme/mpair)

(require "found.rkt")

(define (<<TABLE>>)
  (define tab '())
  (lambda (op . rest)
    (case op
      ((instantiate)
       (let ((table (<<TABLE>>)))
         (mfor-each
          (lambda (elt)
            (table 'put (mcar elt) (eval (mcdr elt))))
          tab)
         table))
      ((copy)
       (let ((table (<<TABLE>>)))
         (mfor-each
          (lambda (elt)
            (table 'put (mcar elt) (mcdr elt)))
          tab)
         table))
      ((get)
       (let* ((key (car rest))
              (entry (massoc key tab)))
         (if entry
             (mcdr entry)
             #f)))
      ((put)
      (let* ((key (car rest))
             (entry (massoc key tab))
             (value (cadr rest)))
        (if entry
            (error "duplicate name" key)
            (set! tab (mcons (mcons key value) tab)))))
      ((replace)
       (let* ((key (car rest))
              (entry (massoc key tab))
              (value (cadr rest)))
         (if entry
             (set-mcdr! entry value)
             (error "undefined name" key))
         #t)))))

(define Root
  (lambda (msg . args)
    (case msg
      ((new)
       (error "cannot instantiate root class"))
      ((<<LOOKUP>>)
       (let*
           ((msg (cadr args)))
         (not-found msg)))
      ((<<COPY>>)
       (let ((tab (<<TABLE>>)))
         (tab 'put '<<SELF>> '())
         tab))
      (else
       (error "invalid class message " msg)))))


(define-macro VAR
  (lambda (name value)
    `(<<VARS>> 'put ',name ',value)))

(define-macro METHOD
  (lambda (msg args . body)
    `(<<METHODS>> 'put ',msg (lambda (<<CONTEXT>> ,@args) ,@body))))
 
(define-macro ?
  (lambda (name)
    `(<<CONTEXT>> 'get ',name)))

(define-macro !
  (lambda (name value)
    `(<<CONTEXT>> 'replace ',name ,value)))

(define-macro CLASS
  (lambda (super . defs)
    `(letrec
         ((<<SUPER>> ,super)
          (<<METHODS>> (<<TABLE>>))
          (<<VARS>> (<<SUPER>> '<<COPY>>))
          (<<CLASS>>
           (lambda (msg . args)
             (case msg
               ((new)
                (let*
                    ((context (<<VARS>> 'instantiate))
                     (self 
                      (lambda (msg . args)
                        (<<CLASS>> '<<EVAL>> context msg args))))
                  (context 'replace '<<SELF>> self)
                  self))
               ((<<EVAL>>)
                (let*
                    ((context (car args))
                     (msg (cadr args))
                     (args (caddr args))
                     (entry (<<METHODS>> 'get msg)))
                  (if entry
                      (apply entry (cons context args))
                      (let ((return (<<SUPER>> '<<LOOKUP>> context msg args)))
                        (if (found? return)
                            (found-value return)
                            (error "method not found " (found-value return))))
                      )))
               ((<<LOOKUP>>)
                (let*
                    ((context (car args))
                     (msg (cadr args))
                     (args (caddr args))
                     (entry (<<METHODS>> 'get msg)))
                  (if entry
                      (found (apply entry (cons context args)))
                      (<<SUPER>> '<<LOOKUP>> context msg args))))
               ((<<COPY>>)
                (<<VARS>> 'copy))
               (else
                (error "invalid class message " msg))))))
       ,@defs
       <<CLASS>>)))
               
(define-macro NEW
  (lambda (class)
    `(,class 'new)))

(define-macro SEND
  (lambda (object msg . args)
    `(,object ',msg ,@args)))

(define-macro SELF
  (lambda ()
    `(<<CONTEXT>> 'get '<<SELF>>)))
  
(define-macro SUPER
  (lambda (msg . args)
    `(<<SUPER>> '<<EVAL>> <<CONTEXT>> ',msg ,args)))

;This is the example
(define Counter
  (CLASS Root
         (VAR count 0)
         (METHOD incr ()
                 (! count (+ (? count) 1)))
         (METHOD decr ()
                 (! count (- (? count) 1)))
         (METHOD value ()
                 (? count))))

(define ProtectedCounter
  (CLASS Counter
         (VAR max 10)
         (METHOD incr ()
                 (if (< (SEND (SELF) value) (? max))
                     (SUPER incr)
                     (display "Counter overflow")))
         (METHOD decr ()
                 (if (> (SEND (SELF) value) 0)
                     (SUPER decr)
                     (display "Counter underflow")))))

(define counter (NEW Counter))
(define protectedCounter (NEW ProtectedCounter))