;;; ic.scm -- Toy Scheme VM suing inline cache technology

#lang racket

(require scheme/mpair)

;;; Compiler
;;
;; Compile Scheme expression to an Stack VM IR

(define (literal? exp)
  (or (string? exp) (number? exp) (char? exp) (boolean? exp)))

(define (primitive? op) (memq op '(%+ %- %<)))

(define (compile exp)
  (cond ((literal? exp) (list (list 'lit exp)))
        ((symbol? exp) (list (list 'var exp)))
        ((and (pair? exp) (eq? (car exp) 'lambda))
         (list (list 'closure (cadr exp) (compile (caddr exp)))))
        ((and (pair? exp) (eq? (car exp) 'if))
         (append (compile (cadr exp))
                 (list (list 'if
                             (compile (caddr exp))
                             (compile (cadddr exp))))))
        ((and (pair? exp) (eq? (car exp) 'let))
         (append (compile-bindings (cadr exp))
                 (list (list 'let (map car (cadr exp))))
                 (compile (caddr exp))))
        ((and (pair? exp) (eq? (car exp) 'set!))
         (append (compile (caddr exp))
                 (list (list 'lit (cadr exp))
                       (list 'set!))))
        ((and (pair? exp) (primitive? (car exp)))
         (append (compile-args (cdr exp))
                 (list (list (car exp)))))
        ((pair? exp)
         (append (compile-args exp)
                 (list (list 'call (length (cdr exp))
                             (make-empty-ic)))))
        (else (error "don't know how to compile expression" exp))))

(define (compile-bindings bindings)
  (if (null? bindings)
      bindings
      (append (compile-bindings (cdr bindings))
              (let ((binding (car bindings)))
                (compile (cadr binding))))))

(define (compile-args args)
  (if (null? args)
      args
      (append (compile-args (cdr args))
              (compile (car args)))))

;;; Interpreter
;;
;; Interpret the VM IR and update the inline caches with the
;; know closure called.

(define (take n ls)
  (if (= n 0)
      '()
      (cons (car ls) (take (- n 1) (cdr ls)))))

(define (drop n ls)
  (if (= n 0)
      ls
      (drop (- n 1) (cdr ls))))

(define (lookup-env name env)
  (let ((entry (assq name env)))
    (if (pair? entry)
        (cdr entry)
        ;; if we didn't find it in the local environemnt
        ;; try the global one
        (let ((global (assq name *global*)))
          (if global
              (cdr global)
              (error "unbound variable" name))))))

(define (bind-env names vals env)
  (if (null? names)
      env
      (cons (cons (car names) (car vals))
            (bind-env (cdr names) (cdr vals) env))))

(define (update-env! name value env)
  (let ((entry (assq name env)))
    (if entry
        (set-mcdr! entry value)
        (let ((global (assq name *global*)))
          (if global
              (set-mcdr! global value)
              (set! *global* (cons (cons name value) *global*)))))))

(define (ev is env)
  (let lp ((stack '())
           (is is)
           (env env))
    ;; for debug (display is) (display stack) (newline)
    (if (null? is)
        (let ((top (car stack)))
          (if (null? (cdr stack))
              top
              (let ((frame (cadr stack)))
                (lp (cons top (cddr stack))
                    (frame-code frame)
                    (frame-env frame)))))
        (let ((i (car is)))
          (cond ((eq? (car i) 'lit)
                 (lp (cons (cadr i) stack)
                     (cdr is) env))
                ((eq? (car i) 'var)
                 (lp (cons (lookup-env (cadr i) env) stack)
                     (cdr is) env))
                ((eq? (car i) 'closure)
                 (lp (cons (make-closure (cadr i) (caddr i) env) stack)
                     (cdr is) env))
                ((eq? (car i) 'if)
                 (if (car stack)
                     (lp (cdr stack) (cadr i) env)
                     (lp (cdr stack) (caddr i) env)))
                ((eq? (car i) 'let)
                 (let* ((n (length (cadr i)))
                        (vals (take n stack)))
                   (lp (drop n stack) (cdr is) (bind-env (cadr i) vals env))))
                ((eq? (car i) 'call)
                 (let ((op (car stack)))
                   (if (closure? op)
                       (let* ((args (closure-args op))
                              (n (length args))
                              (vals (take n (cdr stack)))
                              (stack* (drop n (cdr stack))))
                         ;; Update the inline cache with the called procedure
                         (update-ic! (caddr i) op)
                         (lp (cons (make-frame (cdr is) env) stack*)
                             (closure-code op)
                             (bind-env args vals (closure-env op))))
                       (error "calling non procedure" op))))
                ((eq? (car i) 'set!)
                 (let ((name (car stack))
                       (value (cadr stack)))
                   (update-env! name value env)
                   (lp (cdr stack) (cdr is) env)))
                ((eq? (car i) '%+)
                 (let ((a (car stack)) (b (cadr stack)))
                   (lp (cons (+ a b) (cddr stack)) (cdr is) env)))
                ((eq? (car i) '%-)
                 (let ((a (car stack)) (b (cadr stack)))
                   (lp (cons (- a b) (cddr stack)) (cdr is) env)))
                ((eq? (caar i) '%<)
                 (let ((a (car stack)) (b (cadr stack)))
                   (lp (cons (< a b) (cddr stack)) (cdr is) env)))
                (else (error "unknown instruction " i)))))))

;;; Runtime
;;
;; Runtime representation of Scheme values. For now, use the underlying
;; Scheme system representation except for closures, because we need
;; to track some information for when we optimize the closure. Too bad 
;; there is no way to portably associate an information with a Scheme
;; closure. We may use a table for that, but ...

(define *global* '())

(define (make-closure args code env) (vector 'closure args code env))
(define (closure? o) (and (vector? o) (eq? 'closure (vector-ref o 0))))
(define (closure-args c) (vector-ref c 1))
(define (closure-code c) (vector-ref c 2))
(define (closure-env  c) (vector-ref c 3))

(define (make-empty-ic) (vector 'ic #f))
(define (ic-value ic) (vector-ref ic 1))
(define (update-ic! ic v) (vector-set! ic 1 v))

(define (make-frame code env) (vector 'frame code env))
(define (frame? o) (and (vector? o) (eq? (vector-ref o 0) 'frame)))
(define (frame-code f) (vector-ref f 1))
(define (frame-env f) (vector-ref f 2))

;;; Test

(define (print val)
  (cond ((closure? val) (display "#<closure>"))
        (else (write val))))

(define (test)
  (ev (compile '(set! + (lambda (a b) (%+ a b)))) '())
  (print (ev (compile '(+ 1 2)) '())))
