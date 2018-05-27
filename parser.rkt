#lang racket

(require syntax/strip-context "partial.rkt")

(provide (rename-out [literal-read read]
                     [literal-read-syntax read-syntax]))

(define (parse-partial P prods prod-children start)
  (define (assign-id partial min)
    (define sum (+ 1 min))
    (Partial min
             (Partial-Non-Terminal partial)
             (Partial-Terminal partial)
             (Partial-Filled? partial)
             (for/list ([child (Partial-Children partial)])
               (begin0
                 (assign-id child sum)
                 (set! sum (+ sum (Partial-Size child)))))))

  (define (parse-node node)
    (match node
      [`(??) (Partial -1 #f 'HOLE #f '())]
      [`(,terminal) (Partial -1 #f terminal #t '())]
      [`(,terminal ,children ...)
       (Partial -1
                #f
                terminal
                #t
                (for/list ([child children]) (parse-node child)))]
      [_ (error "Illegal node definition:" node "in" P)]))

  (define (type-check partial type)
    (match-define (struct Partial (id nt terminal filled? children)) partial)
    (if filled?
        (if (ormap (lambda (nt) (eq? nt type)) (dict-ref prods terminal))
            (begin
              (unless (= (length children) (length (dict-ref prod-children terminal)))
                (error "Not enough children at node:" partial))
              (Partial id type terminal filled?
                       (map
                        (lambda (child type) (type-check child type))
                        children
                        (dict-ref prod-children terminal))))
            (error "Program does not type check at node:" (print-node partial) "expected"
                   type "got" (dict-ref prod-children terminal)))
        (Partial id type terminal filled? '())))
  (define parsed (assign-id (parse-node P) 0))
  (define typed (type-check parsed start))
  typed)

(define (parse-spec terminal args out spec)
  (when (check-duplicates args #:default #f)
    (error "Duplicate argument names in spec for terminal:" terminal))

  (define numargs 0)
  (define renamed
    (for/hash ([var args])
      (begin0
        (values var (string->symbol (format "x~s" numargs)))
        (set! numargs (+ numargs 1)))))

  (when (hash-has-key? renamed out)
    (error "Output name is the same as an argument name in spec for terminal:" terminal))

  (define (rename var)
    (if (eq? var out)
        'y
        (dict-ref renamed var var)))
  
  (define (parse-expr e)
    (match e
      [(? number?) e]
      [(? symbol?)
       (define split (regexp-split #rx"\\." (symbol->string e)))
       (if (= (length split) 1)
           (list 'head (rename (string->symbol (car split))))
           (map (lambda (x) (rename (string->symbol x))) (reverse split)))]
      [_ #f]))
  
  (define (parse-rule e)
    (match e
      [`(,lhs ,op ,rhs) (list op (parse-expr lhs) (parse-expr rhs))]))
  (for/list ([rule spec]) (parse-rule rule)))
 
(define (parse file)
  (define rules (make-hash))
  ;; For typing
  (define prods (make-hash))
  (define prod-children (make-hash))
  (define spec (make-hash))
  (define start #f)
  (define partials (make-hash))
  (define progs (make-hash))
  (for ([expr (in-port read file)])
    (match expr
      [`(Terminal ,N ,x ...)
       (unless start (set! start N))
       (dict-set! rules N x)
       (for ([prod x])
         (define term (Production-Terminal prod))
         (dict-set! prods term (cons N (dict-ref prods term '())))
         (dict-set! prod-children term (Production-Children prod)))]
      [`(Spec ,prod ,semantics)
       (match prod
         [`(,terminal ,args ,out)
          (unless (hash-has-key? prods terminal)
            (error "Spec for unknown terminal:" terminal))
          (dict-set! spec terminal (parse-spec terminal args out semantics))]
         [_ #f])]
      [`(Partial ,name ,P)
       (dict-set! partials name (parse-partial P prods prod-children start))]
      [`(Synthesize ,progname [,in ,out] ,partial)
       (unless (and (hash-has-key? partials in)
                    (hash-has-key? partials out)
                    (hash-has-key? partials partial))
         (error "Undefiend partial at Synthesize:" expr))
       (dict-set! progs progname
                  (list (dict-ref partials in)
                        (dict-ref partials out)
                        (dict-ref partials partial)))]))
  (unless start (error "No terminals provided for language"))
  (list prods prod-children spec start partials progs))

(provide (rename-out [literal-read read]
                     [literal-read-syntax read-syntax]))
 
(define (literal-read in)
  (syntax->datum
   (literal-read-syntax #f in)))
 
(define (literal-read-syntax src in)
  (with-syntax ([parsed (parse in)])
    (strip-context
     #'(module anything racket
         (define prog 'parsed)))))