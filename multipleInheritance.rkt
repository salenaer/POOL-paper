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
         (if entry
             (set-mcdr! entry value)
             (set! tab (mcons (mcons key value) tab)))))
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
          rest) ; copy into => if it already exists give error
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

(define-macro CLASS
  (lambda (supers . defs)
    `(letrec
         ((<<SUPERS>> (SUPERS ,supers))
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
               ((<<COPY>>)
                (<<VARS>> 'copy))
               ((<<COPY-INTO>>)
                (<<VARS>> 'copy-into (car args)))
               (else
                (error "invalid multiclass message " msg))))))
       ,@defs
       <<CLASS>>)))
;-------------------------------------------------------------------------------------------------------------------------
(define Person
  (CLASS (list Root)
         (VAR name void)
         (VAR adress void)
         (METHOD set-name (newName)
                 (! name newName))
         (METHOD set-adress (newAdress)
                 (! adress newAdress))
         (METHOD get-name ()
                 (? name))
         (METHOD get-adress ()
                 (? adress))
         (METHOD print ()
                 (display "name: ")(display (? name))(newline)
                 (display "adress: ")(display (? adress))(newline))
         ))

(define Student
  (CLASS (list Person)
         (VAR studentId void)
         (VAR faculty void)
         (METHOD set-studentId (newID)
                 (! studentId newID))
         (METHOD set-faculty (newFaculty)
                 (! faculty newFaculty))
         (METHOD get-studentId ()
                 (? studentId))
         (METHOD get-faculty ()
                 (? faculty))
         (METHOD print ()
                 (SUPER print)
                 (display "studentId: ")(display (? studentId))(newline)
                 (display "faculty: ")(display (? faculty))(newline))
         ))

(define Personnel
  (CLASS (list Person)
         (VAR personnelId void)
         (VAR faculty void)
         (METHOD set-personnelId (newID)
                 (! personnelId newID))
         (METHOD set-faculty (newFaculty)
                 (! faculty newFaculty))
         (METHOD get-personnelId ()
                 (? personnelId))
         (METHOD get-faculty ()
                 (? faculty))
         (METHOD print ()
                 (SUPER print)
                 (display "personnelId: ")(display (? personnelId))(newline)
                 (display "faculty: ")(display (? faculty))(newline))
         ))

(define Assistent
  (CLASS (list Personnel Student)
         (VAR PhDThesis void)
         (METHOD set-PhDThesis  (newPHD)
                 (! PhDThesis  newPHD))
         (METHOD get-PhDThesis ()
                 (? PhDThesis))
         (METHOD print ()
                 (SUPER print)
                 (display "PhDThesis: ")(display (? PhDThesis:))(newline))
         ))

(define noah (NEW Student))
(SEND noah set-name "noah")
(SEND noah set-adress "Merchtem")
(SEND noah set-studentId 500305)
(SEND noah set-faculty "computerscience")

(define andy (NEW Personnel))
(SEND andy set-name "andy")
(SEND andy set-adress "10F732")
(SEND andy set-personnelId 6293581)
(SEND andy set-faculty "computerscience")

(define guillermo (NEW Assistent))
(SEND guillermo set-name "guillermo")
(SEND guillermo set-adress "Pleinlaan 2")
(SEND guillermo set-studentId 123456)
(SEND guillermo set-personnelId 654321)
(SEND guillermo set-faculty "computerscience")