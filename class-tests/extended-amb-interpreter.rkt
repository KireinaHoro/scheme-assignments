#lang racket

(require r5rs)
(print-mpair-curly-braces #f)

(define-syntax expand-rules
  (syntax-rules ()
    [(expand-rules (s f))
     (list (cons (quote s) f))]
    [(expand-rules (s1 f1) (s2 f2) ...)
     (cons (cons (quote s1) f1)
           (expand-rules (s2 f2) ...))]))
(define-syntax-rule (expand-primitives x ...)
  (expand-rules (x x) ...))

(define (my-eval exp env)
  ((analyze exp) env))
(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (analyze exp)
  (cond [(self-evaluating? exp)
         (analyze-self-evaluating exp)]
        [(variable? exp) (analyze-variable exp)]
        [(assq (car exp) analyze-rules)
         => (lambda (type-rule-pair)
              ((cdr type-rule-pair) exp))]
        [(application? exp) (analyze-application exp)]
        [else
         (error "Unknown expression type -- ANALYZE" exp)]))

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))
(define (analyze-quoted exp)
  (let ([qval (text-of-quotation exp)])
    (lambda (env succeed fail)
      (succeed qval fail))))
(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

(define (analyze-assignment exp)
  (let ([var (assignment-variable exp)]
        [vproc (analyze (assignment-value exp))])
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)          ; *1*: success continuation that saves old value
               (let ([old-value
                      (lookup-variable-value var env)])
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()      ; *2*: failure continuation restores the value first
                            (set-variable-value! var
                                                 old-value
                                                 env)
                            (fail2)))))
             fail))))
(define (analyze-definition exp)
  (let ([var (definition-variable exp)]
        [vproc (analyze (definition-value exp))])
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (analyze-if exp)
  (let ([pproc (analyze (if-predicate exp))]
        [cproc (analyze (if-consequent exp))]
        [aproc (analyze (if-alternative exp))])
    (lambda (env succeed fail)
      (pproc env
             ;; success continuation for evaluating the predicate
             ;; to obtain pred-value
             (lambda (pred-value fail2)
               ((if (true? pred-value)
                    cproc
                    aproc) env succeed fail2))
             ;; failure continuation for evaluating the predicate
             fail))))
(define (analyze-and exp)
  (define (my-and terms)
    (lambda (env)
      (cond [(null? terms) true]
            [(null? (rest-terms terms))
             ((analyze (first-term terms)) env)]
            [(false? ((analyze (first-term terms)) env)) false]
            [else ((my-and (rest-terms terms)) env)])))
  (my-and (cdr exp)))
(define (analyze-or exp)
  (define (my-or terms)
    (lambda (env)
      (if (null? terms) false
          (let ([first ((analyze (first-term terms)) env)])
            (if (true? first)
                first
                ((my-or (rest-terms terms)) env))))))
  (my-or (cdr exp)))
(define (analyze-lambda exp)
  (let ([vars (lambda-parameters exp)]
        [bproc (analyze-sequence (lambda-body exp))])
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))
(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; success continuation for calling a
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;; failure continuation for calling a
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ([procs (map analyze exps)])
    (when (null? procs)
      (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))
(define (analyze-application exp)
  (let ([fproc (analyze (operator exp))]
        [aprocs (map analyze (operands exp))])
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))
(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env
                    ;; success continuation for this aproc
                    (lambda (arg fail2)
                      (get-args (cdr aprocs)
                                env
                                ;; success continuation for recursive
                                ;; call to get-args
                                (lambda (args fail3)
                                  (succeed (cons arg args)
                                           fail3))
                                fail2))
                    fail)))
(define (execute-application proc args succeed fail)
  (cond [(primitive-procedure? proc)
         (succeed (apply proc args)
                  fail)]
        [(compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail)]
        [else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION" proc)]))

(define (analyze-amb exp)
  (let ([cprocs (map analyze (amb-choices exp))])
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
                           succeed
                           (lambda ()
                             (try-next (cdr choices))))))
      (try-next cprocs))))

(define (analyze-require exp)
  (let ([pproc (analyze (require-predicate exp))])
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (not (true? pred-value))
                   (fail2)
                   (succeed 'ok fail2)))
             fail))))

(define (analyze-if-fail exp)
  (let ([pproc (analyze (if-fail-primary exp))]
        [aproc (analyze (if-fail-alternative exp))])
    (lambda (env succeed fail)
      (pproc env
             (lambda (value fail2)
               (succeed value fail))    ; call succeed as usual
             (lambda () ; evaluate the second part
               (aproc env
                      succeed
                      fail))))))

(define (analyze-all-answer exp)
  (let ([proc (analyze (all-answer-exp exp))])
    (lambda (env succeed fail)
      (proc env
            (lambda (single-val fail2)
              (displayln single-val)
              (fail2))
            (lambda ()
              (succeed 'ok fail))))))

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

(define (require? exp) (tagged-list? exp 'require))
(define (require-predicate exp) (cadr exp))

(define (if-fail? exp) (tagged-list? exp 'if-fail))
(define (if-fail-primary exp) (cadr exp))
(define (if-fail-alternative exp) (caddr exp))

(define (all-answer? exp) (tagged-list? exp 'all-answer))
(define (all-answer-exp exp) (cadr exp))

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

(define analyze-rules
  (expand-rules (quote analyze-quoted)
                (set! analyze-assignment)
                (define analyze-definition)
                (if analyze-if)
                (and analyze-and)
                (or analyze-or)
                (lambda analyze-lambda)
                (amb analyze-amb)
                (require analyze-require)
                (if-fail analyze-if-fail)
                (all-answer analyze-all-answer)
                (begin (compose1 analyze-sequence begin-actions))
                (cond (compose1 analyze cond->if))
                (let (compose1 analyze-application let->application))
                (let* (compose1 analyze let*->nested-lets))))

(define primitive-procedures
  (expand-primitives car cdr
                     caar cadr cdar cddr
                     caaar caadr cadar caddr cdaar cdadr cddar cdddr
                     cons null? void                  ;; pair operations
                     + - * / > < = not remainder sqrt ;; math operations
                     even? odd?                       ;; math cont.
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
  (define (internal-loop try-again)
    (let ([input (read)])
      (cond [(eq? input 'try-again)
             (try-again)]
            [(eq? input eof) (void)]
            [else
             (ambeval input
                      the-global-environment
                      ;; ambeval success
                      (lambda (val next-alternative)
                        (user-print val)
                        (internal-loop next-alternative))
                      ;; ambeval failure
                      (lambda ()
                        (displayln "There are no more answers.")
                        (driver-loop)))])))
  (internal-loop
   (lambda ()
     (displayln "There are no more answers.")
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