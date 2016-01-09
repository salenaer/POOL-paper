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
      ((copy-into)                 ;aangepast
       (let ((table (car rest)))
         (mfor-each
          (lambda (elt)
            (table 'put-or-replace (mcar elt) (mcdr elt)))
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
         #t))
      ((put-or-replace) ;aangepast
       (let* ((key (car rest))
              (entry (massoc key tab))
              (value (cadr rest)))
         (cond ((and entry (eq? (mcdr entry) value)) 'do-nothing)
               (entry (error "dupicate variable with different value for:" key "original value:" (mcdr entry) "newvalue:" value))
               (else (set! tab (mcons (mcons key value) tab))))))
      )))

(define Root
  (lambda (msg . args)
    (case msg
      ((new)
       (error "cannot instantiate root class"))
      ((<<LOOKUP>>)
       (let*
           ((msg (cadr args)))
         (not-found msg)))
      ((<<SPECIFIC-LOOKUP>>) ;aangepast
       (let*
           ((msg (caddr args)))
         (not-found msg)))
      ((<<COPY>>)
       (let ((tab (<<TABLE>>)))
         (tab 'put '<<SELF>> '())
         tab))
      ((name)
       "root")
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
  (lambda (target msg . args)
    `(<<SUPERS>> '<<EVAL>> ',target <<CONTEXT>> ',msg ,args))) ;aangepast

;--------------------------------------------------------------------------------------------------------------------
(define (super-lookup supers function fail)
  (define (iter lst)
    (if (null? lst)
        (fail)
        (let ((entry (function (car lst))))
          (if (found? entry)
              entry
              (iter (cdr lst))))))
  (iter supers))

(define (SUPERS input)
  (let ((sups input))
  (lambda (op . args)
    (case op
      ((<<LOOKUP>>)
       (let
           ((context (car args))
            (msg (cadr args))
            (args (caddr args)))
         (super-lookup sups
                       (lambda (someSuper) (someSuper '<<LOOKUP>> context msg args))
                       (lambda ()(not-found msg)))))        
      ((<<COPY>>)
       (let ((table ((car sups) '<<COPY>>))
             (rest (cdr sups)))
         (for-each
          (lambda (some-super)
            (set! table (some-super '<<COPY-INTO>> table)))
          rest) ; copy into => if it already exists give error
         table))
      ((<<EVAL>>)
       (let*
           ((target (car args)) ;aangepast
            (context (cadr args))
            (msg (caddr args))
            (args (cadddr args))
            (return (super-lookup sups
                                  (lambda (someSuper) (someSuper '<<SPECIFIC-LOOKUP>> target context msg args))
                                  (lambda ()(not-found msg)))))
         (if (found? return)
             (found-value return)
             (error "method not found " (found-value return)))))
      ((<<SPECIFIC-LOOKUP>>) ;aangepast
       (let*
           ((target (car args))
            (context (cadr args))
            (msg (caddr args))
            (args (cadddr args)))
            (super-lookup sups
                                  (lambda (someSuper) (someSuper '<<SPECIFIC-LOOKUP>> target context msg args))
                                  (lambda ()(not-found msg)))))
      (else
       (error "invalid supers message " op))))))

(define-macro CLASS
  (lambda (name supers . defs)
    `(letrec
         ((<<SUPERS>> (SUPERS (list ,@supers)))
          (<<NAME>> ',name)
          (<<METHODS>> (<<TABLE>>))
          (<<VARS>> (<<SUPERS>> '<<COPY>>))
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
                      (let ((return (<<SUPERS>> '<<LOOKUP>> context msg args)))
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
                      (<<SUPERS>> '<<LOOKUP>> context msg args))))
               ((<<SPECIFIC-LOOKUP>>) ;aangepast
                (let*
                    ((target (car args))
                     (context (cadr args))
                     (msg (caddr args))
                     (args (cadddr args)))
                (if (eq? target <<NAME>>)
                      (<<CLASS>> '<<LOOKUP>> context msg args)
                      (<<SUPERS>> '<<SPECIFIC-LOOKUP>> target context msg args))))
               ((<<COPY>>)
                (<<VARS>> 'copy))
               ((<<COPY-INTO>>)
                (<<VARS>> 'copy-into (car args)))
               ((name)
                <<NAME>>)
               (else
                (error "invalid multiclass message " msg))))))
       ,@defs
       <<CLASS>>)))
;-------------------------------------------------------------------------------------------------------------------------
(define Counter
  (CLASS 
   Counter
   (Root)
   (VAR count 0)
   (METHOD incr ()
           (! count (+ (? count) 1)))
   (METHOD decr ()
           (! count (- (? count) 1)))
   (METHOD value ()
           (? count))))

(define ProtectedCounter
  (CLASS 
   ProtectedCounter
   (Counter)
   (VAR max 10)
   (METHOD incr ()
           (if (< (SEND (SELF) value) (? max))
               (SUPER "Counter" incr)
               (display "Counter overflow")))
   (METHOD decr ()
           (if (> (SEND (SELF) value) 0)
               (SUPER "Counter" decr)
               (display "Counter underflow")))))

(define counter (NEW Counter))
(SEND counter incr)
(SEND counter incr)
(SEND counter value)
(SEND counter decr)
(SEND counter value)