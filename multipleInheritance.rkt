(require mzlib/defmacro)
(require scheme/mpair)
(require racket/list)

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
    `(<<SUPERS>> '<<EVAL>> <<CONTEXT>> ',msg ,args))) ;aangepast

;--------------------------------------------------------------------------------------------------------------------
(define (super-lookup supers context msg args)
  (define (iter lst)
    (if (null? lst)
        (not-found msg)
        (let ((entry ((car lst) '<<LOOKUP>> context msg args)))
          (if (found? entry)
              entry
              (iter (cdr lst))))))
  (iter supers))

(define (SUPERS input)
  (let ((sups (reverse input)))
    (lambda (op . args)
      (case op
        ((<<LOOKUP>>)
         (let
             ((context (car args))
              (msg (cadr args))
              (args (caddr args)))
           (super-lookup sups context msg args)))         
        ((<<COPY>>)
         (let ((table ((car sups) '<<COPY>>))
               (rest (cdr sups)))
           (for-each
            (lambda (some-super)
              (set! table (some-super '<<COPY-INTO>> table)))
            rest)
           table))
        ((<<EVAL>>)
         (let*
             ((context (car args))
              (msg (cadr args))
              (args (caddr args))
              (return (super-lookup sups context msg args)))
           (if (found? return)
               (found-value return)
               (error "method not found " (found-value return)))))
        (else
         (error "invalid supers message " op))))))

(define (rename lst class)
  (define (translate msg)
    (let ((entry (assoc msg lst)))
      (if entry
          (cdr entry)
          msg)))
  (let ((<<RENAMEDCLASS>>
        (lambda (msg . args)
          (case msg
            ((<<LOOKUP>>)
             (let*
                 ((context (car args))
                  (msg (cadr args))
                  (args (caddr args)))
               (class '<<LOOKUP>> context (translate msg) args)))
            ((<<EVAL>>)
             (let*
                 ((context (car args))
                  (msg (cadr args))
                  (args (caddr args)))
               (class '<<EVAL>> context (translate msg) args)))
            (else
             ; . args zet args in een lijst
             ; class msg args voert functie uit met args in lijst
             ; apply gaat functie uitvoeren op args
             (apply class msg args)))))) 
    <<RENAMEDCLASS>>))

(define (flatten lst)
  (define (loop lst iter)
    (if (null? lst)
        iter
        (loop (cdr lst) (cons (caar lst) (cons (cdar lst) iter)))))
  (loop lst '()))

(define-macro (RENAME lst class)
  `(rename
    (flatten (list ,@lst))
    ,class))

(define-macro BY
  (lambda (a b)
  `(cons (cons ',a ',b)(cons ',b ',a))))

(define-macro CLASS
  (lambda (supers . defs)
    `(letrec
         ((<<SUPERS>> (SUPERS (list ,@supers))) ;aangepast
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
               ((<<EVAL>>) ;aangepast
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
                            (error "method not found " (found-value return)))))))
               ((<<LOOKUP>>) ;aangepast
                (let*
                    ((context (car args))
                     (msg (cadr args))
                     (args (caddr args))
                     (entry (<<METHODS>> 'get msg)))
                  (if entry
                      (found (apply entry (cons context args)))
                      (<<SUPERS>> '<<LOOKUP>> context msg args))))
               ((<<COPY>>)
                (<<VARS>> 'copy))
               ((<<COPY-INTO>>)
                (<<VARS>> 'copy-into (car args)))
               (else
                (error "invalid multiclass message " msg))))))
       ,@defs
       <<CLASS>>)))
;-------------------------------------------------------------------------------------------------------------------------
(define A
  (CLASS (Root)
         (METHOD print ()
              (display "print of A called")(newline))
         ))

(define B
  (CLASS (Root)
         (METHOD print ()
              (display "print of B called")(newline))
         ))
(define Child
  (CLASS ((RENAME ((BY print aPrint)) A)
          (RENAME ((BY print bPrint)) B))
         (METHOD printWithA ()
                 (SUPER aPrint)
                 (SEND (SELF) print))
         (METHOD printWithB ()
                 (SUPER bPrint)
                 (SEND (SELF) print))
         (METHOD printWithBoth ()
                 (SUPER aPrint)
                 (SUPER bPrint)
                 (SEND (SELF) print))
         (METHOD print ()
                 (display "print of C called")(newline))

         ))

(define josh (NEW Child))