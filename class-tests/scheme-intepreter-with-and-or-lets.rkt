#lang racket
(require r5rs)
(print-mpair-curly-braces #f)

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
         (my-apply (my-eval (operator exp) env)
                   (list-of-values (operands exp) env))]
        [else
         (error "Uknown expression type -- EVAL" exp)]))

;; apply (different name so as not to shadow #<procedure:apply> from racket/base)
(define (my-apply procedure arguments)
  (cond [(primitive-procedure? procedure)
         (apply procedure arguments)]
        [(compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure)))]
        [else
         (error "Unknown procedure type -- APPLY" procedure)]))

;; evaluate list of values
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (my-eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;; evaluate expressions

(define (eval-if exp env)
  (if (true? (my-eval (if-predicate exp) env))
      (my-eval (if-consequent exp) env)
      (my-eval (if-alternative exp) env)))

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
      (let ([output (my-eval input the-global-environment)])
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