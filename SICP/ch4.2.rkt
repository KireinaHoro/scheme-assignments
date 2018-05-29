#lang racket

(require r5rs)
(print-mpair-curly-braces #f)

(define (zip . ls)
  (cond [(null? ls) '()]
        [(null? (car ls)) '()]
        [else (cons (map car ls)
                    (apply zip (cdr ls)))]))

;; Ex4.3 -- eval in data-directed approach
(define (my-eval exp env)
  (cond [(self-evaluating? exp) exp]
        [(variable? exp) (lookup-variable-value exp env)]
        ;; eval-rules is an assoc list of
        ;; (type . rule) pairs.
        ;; type is a symbol ('quote, 'define, 'lambda etc.)
        ;; rule must be a procedure that accepts exp and env
        [(assq (car exp) eval-rules) => (lambda (type-rule-pair)
                                          ((cdr type-rule-pair) exp env))]
        [(application? exp)
         (my-apply (actual-value (operator exp) env)
                   (operands exp)
                   env)]
        [else
         (error "Unknown expression type -- EVAL" exp)]))

;; thunks
(define (actual-value exp env)
  (force-it (my-eval exp env)))

;; apply (different name so as not to shadow #<procedure:apply> from racket/base)
(define (my-apply procedure arguments env)
  (cond [(primitive-procedure? procedure)
         (apply procedure
                (list-of-arg-values arguments env))] ;; instant
        [(compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (map (lambda (param)
                  (if (and (pair? param)
                           (let ([tag (cadr param)])
                             (or (eq? tag 'lazy)
                                 (eq? tag 'lazy-memo))))
                      (car param)
                      param))
                (procedure-parameters procedure))
           (list-of-args (procedure-parameters procedure)
                         arguments env)              ;; mixed
           (procedure-environment procedure)))]
        [else
         (error "Unknown procedure type -- APPLY" procedure)]))

;; parameter list with mixed values -- forced, delayed, delayed-memoize
(define (list-of-args params exps env)
  (if (no-operands? exps)
      '()
      (map (lambda (param exp)
             (if (pair? param)
                 (delay-it
                  (if (eq? 'lazy (cadr param))
                      'simple
                      'memoize)
                  exp env)
                 (actual-value exp env)))
           params exps)))

;; evaluate list of values -- force evaluation
(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))

;; evaluate expressions

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (my-eval (if-consequent exp) env)
      (my-eval (if-alternative exp) env)))

;; Ex4.4
(define (eval-and exp env)
  (define (my-and terms)
    (cond [(null? terms) true]
          [(null? (rest-terms terms)) (my-eval (first-term terms) env)]
          [(false? (my-eval (first-term terms) env)) false]
          [else (my-and (rest-terms terms))]))
  (my-and (cdr exp)))
(define (eval-or exp env)
  (define (my-or terms)
    (if (null? terms) false
        (let ([first (my-eval (first-term terms) env)])
          (if (true? first)
              first
              (my-or (rest-terms terms))))))
  (my-or (cdr exp)))
                     
(define (eval-sequence exps env)
  (cond [(null? exps) (void)]
        [(last-exp? exps) (my-eval (first-exp exps) env)]
        [else (my-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env)]))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (my-eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (my-eval (definition-value exp) env)
    env)
  'ok)

;; represent expressions

(define (self-evaluating? exp)
  (cond [(number? exp) true]
        [(string? exp) true]
        [else false]))
(define (variable? exp) (symbol? exp))
(define (text-of-quotation exp) (cadr exp))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)    ; formal parameters
                   (cddr exp))))  ; body -- can have multiple expressions

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;; structure for and / or
(define (first-term terms) (car terms))
(define (rest-terms terms) (cdr terms))

(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond [(null? seq) seq]
        [(last-exp? seq) (first-exp seq)]
        [else (make-begin seq)]))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (make-application operator operands)
  (cons operator operands))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

;; Ex4.5 -- "=>" syntax in cond
;; with (cond [test]) support as well
(define (expand-clauses clauses)
  (if (null? clauses) 'false          ; we don't have any clauses
      (let ([rest (cdr clauses)])
        (cond
          [(null? (cdar clauses))                ; a clause with only a test
           (let ([test (caar clauses)])
             (make-application
              (make-lambda '(placeholder)
                           (list (make-if 'placeholder
                                          'placeholder
                                          (expand-clauses rest))))
              (list test)))]
          [(eq? (cadar clauses) '=>)
           (let ([test (caar clauses)]
                 [recipient (caddar clauses)])
             (make-application
              (make-lambda '(placeholder)
                           (list (make-if 'placeholder
                                          (make-application recipient (list 'placeholder))
                                          (expand-clauses rest))))
              (list test)))]
          [else 
           (let ([first (car clauses)]
                 [rest (cdr clauses)])
             (if (cond-else-clause? first)
                 (if (null? rest)
                     (sequence->exp (cond-actions first))
                     (error "ELSE clause isn't last -- COND->IF" clauses))
                 (make-if (cond-predicate first)
                          (sequence->exp (cond-actions first))
                          (expand-clauses rest))))]))))

;; Ex4.2
;; putting applications before assignments will make define and set! functions
;; applications that begin with call enables treating define and set! as keywords

;; Ex4.6 -- let
;; Ex4.8 -- named let
(define (deep-map f l)
  (define (deep x)
    (cond [(null? x) x]
          [(pair? x) (map deep (f x))]
          [else (f x)]))
  (map deep l))
(define (let->application exp)
  (let* ([bindings
          (if (symbol? (cadr exp))
              (cons (list (cadr exp) '*unassigned*)
                    (caddr exp))
              (cadr exp))]
         [formals (map car bindings)]
         [arguments (map cadr bindings)]
         [actions (if (symbol? (cadr exp))
                      (cdddr exp)
                      (cddr exp))]
         [patched-actions
          (if (not (symbol? (cadr exp))) actions
              (deep-map (lambda (action)
                          (if (and (pair? action) (eq? (car action) (cadr exp)))
                              (cons (car action) (cons (cadr exp)
                                                       (cdr action)))
                              action))
                        actions))])
    (let ([ll (make-lambda formals patched-actions)])
      (when (symbol? (cadr exp))
        (set-car! arguments ll))     ; the first argument should be the procedure itself
      (make-application ll arguments))))
(define (make-let bindings operations)
  (cons 'let (cons bindings operations)))

;; Ex4.7 -- let*
(define (let*->nested-lets exp)
  (define actions (cddr exp))
  (define (expand-bindings bindings)
    (if (or (null? bindings)
            (null? (cdr bindings)))
        (make-let bindings actions)
        (make-let (list (car bindings))
                  (list (expand-bindings (cdr bindings))))))
  (expand-bindings (cadr exp)))

;; evaluator data structures
(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
(define primitive-procedure? procedure?)
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))
(define (make-procedure parameters body environment)
  (list 'procedure parameters body environment))

;; thunks
(define (force-it-simple obj)
  (actual-value (thunk-exp obj) (thunk-env obj)))
(define (delay-it mode exp env)
  (list mode exp env))

(define (simple-thunk? obj)
  (tagged-list? obj 'simple))

(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))
(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated))
(define (unevaluated-thunk? obj)
  (tagged-list? obj 'memoize))
(define (memoize-thunk? obj)
  (or (evaluated-thunk? obj)
      (unevaluated-thunk? obj)))

(define (force-it-memoize obj)
  (cond [(unevaluated-thunk? obj)
         (let ([result (actual-value (thunk-exp obj)
                                     (thunk-env obj))])
           (set-car! obj 'evaluated)       ;; replace 'thunk with 'evaluated
           (set-car! (cdr obj) result)     ;; replace exp with its value
           (set-cdr! (cdr obj) '())        ;; remove unneeded env
           result)]
        [(evaluated-thunk? obj)
         (thunk-value obj)]))

(define (force-it obj)
  ((cond [(simple-thunk? obj) force-it-simple]
         [(memoize-thunk? obj) force-it-memoize]
         [else (lambda (x) x)])
   obj))

;; environments
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond [(null? vars)
             (env-loop (enclosing-environment env))]
            [(eq? var (car vars))
             (car vals)]
            [else (scan (cdr vars) (cdr vals))]))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ([frame (first-frame env)])
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond [(null? vars)
             (env-loop (enclosing-environment env))]
            [(eq? var (car vars))
             (set-car! vals val)]
            [else (scan (cdr vars) (cdr vals))]))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ([frame (first-frame env)])
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
(define (define-variable! var val env)
  (let ([frame (first-frame env)])
    (define (scan vars vals)
      (cond [(null? vars)
             (add-binding-to-frame! var val frame)]
            [(eq? var (car vars))
             (set-car! vals val)]
            [else (scan (cdr vars) (cdr vals))]))
    (scan (frame-variables frame)
          (frame-values frame))))

;; syntax rules
(define eval-rules
  (list (cons 'quote (lambda (exp env) (text-of-quotation exp)))
        (cons 'set! eval-assignment)
        (cons 'define eval-definition)
        (cons 'if eval-if)
        (cons 'and eval-and)
        (cons 'or eval-or)
        (cons 'lambda (lambda (exp env)
                        (make-procedure (lambda-parameters exp)
                                        (lambda-body exp)
                                        env)))
        (cons 'begin (lambda (exp env) (eval-sequence (begin-actions exp)
                                                      env)))
        (cons 'let* (lambda (exp env) (my-eval (let*->nested-lets exp) env)))
        (cons 'let (lambda (exp env) (my-eval (let->application exp) env)))
        (cons 'cond (lambda (exp env) (my-eval (cond->if exp) env)))))

;; environment setup
(define-syntax explode-objects
  (syntax-rules ()
    [(explode-objects x) (list (cons (quote x) x))]
    [(explode-objects x y ...) (cons (cons (quote x) x)
                                     (explode-objects y ...))]))
(define primitive-procedures
  (explode-objects car cdr
                   caar cadr cdar cddr
                   caaar caadr cadar caddr cdaar cdadr cddar cdddr
                   cons null? void                  ;; pair operations
                   + - * / > < = not remainder sqrt ;; math operations
                   eq? number? symbol? pair?        ;; predicates
                   length list append assoc         ;; list operations
                   newline display displayln        ;; output operations
                   ))

(define (primitive-procedure-names) (map car primitive-procedures))
(define (primitive-procedure-objects) (map cdr primitive-procedures))

(define (setup-environment)
  (let ([initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)])
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))

(define (driver-loop)
  (let ([input (read)])
    (unless (eq? eof input)
      (let ([output (actual-value input the-global-environment)])
        (user-print output))
      (driver-loop))))
(define (user-print object)
  (cond [(eq? object 'ok) (void)]
        [(eq? object (void)) (void)]
        [(compound-procedure? object)
         (displayln (list 'compound-procedure
                          (procedure-parameters object)
                          (procedure-body object)
                          '<procedure-env>))]
        [else (displayln object)]))

(driver-loop)

;; Ex4.14 -- system map assumes Scheme procedures for applying, but we have different ways of
;; applying procedures to arguments from Scheme
