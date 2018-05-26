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
  (print-partial (Make-Partial-Tree typed))
  typed)

(define (parse-spec rules terminal spec)
  (define (parse-expr e)
    (eprintf "~s\n" e)
    (match e
      [`(,x . ,y) (eprintf "~s ~s\n" x y)]
      [`(,x) (eprintf "~s\n" x)])
    #f)
  (define (parse-rule e)
    (match e
      [`(,lhs ,op ,rhs) (eprintf "Op: ~s\n" op) (parse-expr `(,lhs)) (parse-expr `(,rhs))]))
  (for ([rule spec]) (parse-rule rule))
  #f)
 
(define (parse file)
  (define rules (make-hash))
  ;; For typing
  (define prods (make-hash))
  (define prod-children (make-hash))
  (define spec (make-hash))
  (define start #f)
  (define partials '())
  (for ([expr (in-port read file)])
    (match expr
      [`(Terminal ,N ,x ...)
       (unless start (set! start N))
       (dict-set! rules N x)
       (for ([prod x])
         (define term (Production-Terminal prod))
         (dict-set! prods term (cons N (dict-ref prods term '())))
         (dict-set! prod-children term (Production-Children prod)))]
      [`(Spec ,terminal ,semantics)
       (unless (hash-has-key? prods terminal)
         (error "Spec for unknown terminal:" terminal))
       (dict-set! spec terminal (parse-spec rules terminal semantics))]
      [`(Partial ,P)
       (set! partials (cons partials (parse-partial P prods prod-children start)))]))
  (unless start (error "No terminals provided for language")))

(provide (rename-out [literal-read read]
                     [literal-read-syntax read-syntax]))
 
(define (literal-read in)
  (syntax->datum
   (literal-read-syntax #f in)))
 
(define (literal-read-syntax src in)
  (with-syntax ([parsed (parse in)])
    (strip-context
     #'(module anything racket
         'parsed))))