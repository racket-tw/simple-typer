#lang racket/base

(require nanopass/base
         racket/match)

(define (var? x)
  (symbol? x))
(define (constant? c)
  (or (number? c)
      (string? c)
      (char? c)))

(define-language ST
  (entry Stmt)
  (terminals
   (var (x param))
   (constant (c)))
  (Stmt (stmt)
        ;; bind type
        (: x t)
        ;; define
        (:= x e))
  (Expr (e)
        x
        c
        (λ ([param* t*] ...) t e)
        (e e* ...))
  (Typ (t)
       x
       (-> t* ... t)))

(define-language L1
  (extends ST)
  (Stmt (stmt)
    (- (: x t))))

(struct env (cur parent) #:transparent)
(define (make-env)
  (env (make-hash) (cur-env)))
(define cur-env (make-parameter (env (make-hash) #f)))

(define-parser parse-ST ST)

(define (bind id typ)
  (let ([cur-binding (env-cur (cur-env))])
    (when (hash-ref cur-binding id #f)
      (error 'semantic "cannot rebind: `~a`" id))
    (hash-set! cur-binding id typ)))
(define (lookup id)
  (let ([parent? (env-parent (cur-env))]
        [cur-binding (env-cur (cur-env))])
    (hash-ref cur-binding id
              (if parent?
                  (parameterize ([cur-env parent?])
                    (lookup id))
                  #f))))

(define (ty-eq? t1 t2)
  (unless (equal? t1 t2)
    (error 'semantic "expected: ~a got: ~a" t1 t2)))

(define-pass bind-type* : ST (s) -> * ()
  (Stmt : Stmt (s) -> * ()
        [(: ,x ,t)
         (bind x (unparse-ST t))
         #f]
        [else #t]))
(define-pass ST->L1 : ST (s) -> L1 ()
  (Stmt : Stmt (s) -> Stmt ()
    [(: ,x ,t) (error 'unreachable)]))
(define-pass infer-and-bind-type* : L1 (s) -> L1 ()
  (Stmt : Stmt (s) -> Stmt ()
        [(:= ,x ,e)
         (if (lookup x)
             (ty-eq? (lookup x) (infer e))
             (bind x (infer e)))
         s]))
(define-pass infer : L1 (e) -> * ()
  (Expr : Expr (e) -> * ()
        [(λ ([,param* ,t*] ...) ,t ,e)
         (parameterize ([cur-env (make-env)])
           (for ([p param*]
                 [t t*])
             (bind p t))
           (ty-eq? t (infer e)))
         `(-> ,@t* ,t)]
        [(,e ,e* ...)
         (match (infer e)
           [`(-> ,t* ... ,t)
            (for-each (λ (t e) (ty-eq? t (infer e)))
                      t* e*)
            t]
           [else (error 'semantic "not a funciton: ~a" e)])]
        [,x (lookup x)]
        [,c (cond
              [(number? c) 'number]
              [(string? c) 'string]
              [(char? c) 'char])])
  (Expr e))

(define (all x)
  (let ([st (parse-ST x)])
    (when (bind-type* st)
      ((compose infer-and-bind-type*
                ST->L1) st))))

(all '(: a number))
(all '(:= a 1))
(all '(: id-number (-> number number)))
(all '(:= id-number (λ ([n number]) number n)))
(all '(:= result (id-number a)))

(lookup 'result)
