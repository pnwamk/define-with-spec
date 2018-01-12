#lang racket/base

(require racket/match
         racket/unsafe/ops
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     racket/list
                     racket/sequence))

(provide
 ;; definition forms
 define/spec
 struct/spec
 ;; spec constructors/combinators
 -> either both except listof any)

(define-for-syntax define-with-spec-enforcement (make-parameter #t))
(provide (for-syntax define-with-spec-enforcement))

(define (ending n)
  (case (remainder n 100)
    [(11 12 13) "th"]
    [else
     (case (remainder n 10)
       [(1) "st"]
       [(2) "nd"]
       [(3) "rd"]
       [else "th"])]))

(define-syntax (-> stx)
  (raise-syntax-error '-> "only valid in define/spec specification" stx))
(define-syntax (either stx)
  (raise-syntax-error 'either "only valid in define/spec specification" stx))
(define-syntax (both stx)
  (raise-syntax-error 'both "only valid in define/spec specification" stx))
(define-syntax (except stx)
  (raise-syntax-error 'except "only valid in define/spec specification" stx))
(define-syntax (listof stx)
  (raise-syntax-error 'listof "only valid in define/spec specification" stx))
(define-syntax (any stx)
  (raise-syntax-error 'any "only valid in define/spec specification" stx))

(begin-for-syntax
  (define-syntax-class spec
    #:attributes (pred)
    (pattern (~literal any)
             #:attr pred #'(λ (x) #t))
    (pattern pred:id)
    (pattern ((~literal either) ps:spec ...)
             #:attr pred #'(λ (x) (or (ps.pred x) ...)))
    (pattern ((~literal both) ps:spec ...)
             #:attr pred #'(λ (x) (and (ps.pred x) ...)))
    (pattern ((~literal except) p:spec)
             #:attr pred #'(λ (x) (not (p.pred x))))
    (pattern ((~literal cons) a:spec d:spec)
             #:attr pred #'(λ (x) (and (pair? x) (a.pred (car x)) (d.pred (cdr x)))))
    (pattern ((~literal list) positional:spec ...)
             #:attr pred #'(λ (x) (match x [(list (? positional.pred) ...) #t] [_ #f])))
    (pattern ((~literal listof) elem:spec)
             #:attr pred #'(λ (x) (and (list? x) (andmap elem.pred x))))
    (pattern (~or val:boolean (~and val ((~literal quote) _:id)))
             #:attr pred #'(λ (x) (eqv? x val)))))


(define-syntax (define/spec stx)
  (syntax-parse stx
    [(_ (name:id arg:id ...)
        ((~literal ->) arg-spec:spec ...
                       rng-spec:spec)
        . body)
     (define arg-count (length (syntax->list #'(arg ...))))
     (define spec-count (length (syntax->list #'(arg-spec ...))))
     (unless (= arg-count spec-count)
       (raise-syntax-error
        'def/spec
        (format "Expected ~a argument predicates, given ~a."
                arg-count
                spec-count)))
     (cond
       [(define-with-spec-enforcement)
        (with-syntax*
            ([(provided-arg ...)
              (generate-temporaries #'(arg ...))]
             [fun-name (generate-temporary #'name)]
             [raw-definition (syntax/loc stx (define (fun-name arg ...) . body))]
             [error-name (or (syntax-property #'name 'define/spec-error-name)
                             (syntax-e #'name))])
          (quasisyntax/loc stx
            (begin
              raw-definition
              (define-syntax name
                (λ (usage-stx)
                  (syntax-case usage-stx ()
                    [(_ provided-arg ...)
                     (with-syntax*
                         ([(arg-error (... ...))
                           (for/list ([i (in-range 1 (add1 #,arg-count))]
                                      [a (in-syntax #'(arg ...))]
                                      [given (in-syntax #'(provided-arg ...))]
                                      [a-spec (in-syntax #'(arg-spec ...))])
                             (with-syntax ([a-spec a-spec]
                                           [a a]
                                           [i i]
                                           [src (syntax-source usage-stx)]
                                           [line (syntax-line usage-stx)]
                                           [col (syntax-column usage-stx)])
                               (quasisyntax/loc given
                                 (error 'error-name
                                        "~a:~a:~a\n ~a~a argument to ~a failed predicate!\n ~a\n ~a"
                                        src
                                        line
                                        col
                                        i
                                        (ending i)
                                        'error-name
                                        (format "Expected: ~a" 'a-spec)
                                        (format "Given: ~v" a)))))]
                          [(spec (... ...)) #'(arg-spec.pred ...)]
                          [(a (... ...)) #'(arg ...)]
                          [result (generate-temporary)]
                          [range-error
                           (with-syntax ([src (syntax-source usage-stx)]
                                         [line (syntax-line usage-stx)]
                                         [col (syntax-column usage-stx)])
                             (syntax/loc usage-stx
                               (error 'error-name
                                      "~a ~a:~a\n ~a returned invalid result!\n ~a\n ~a\n ~a"
                                      'src
                                      line
                                      col
                                      'error-name
                                      (format "Promised: ~a" 'rng-spec)
                                      (format "Returned: ~v" result)
                                      (format "Argument list: ~v"
                                              (list arg ...)))))])
                       (syntax/loc usage-stx
                         (let ([arg provided-arg] ...)
                           (unless (spec a)
                             arg-error)
                           (... ...)
                           (define result (fun-name arg ...))
                           (unless (rng-spec.pred result)
                             range-error)
                           result)))]
                    [(_ . wrong-number-of-args)
                     (raise-syntax-error 'error-name
                                         (format "Expected ~a arguments, given ~a"
                                                 #,arg-count
                                                 (length (syntax->list #'wrong-number-of-args)))
                                         usage-stx)]
                    [usage
                     (identifier? #'usage)
                     (with-syntax ([lambda-body
                                    (syntax/loc #'usage
                                      (name arg ...))])
                       #'(λ (arg ...) lambda-body))]))))))]
       [else
        (syntax/loc stx
          (define (name arg ...) . body))])]))



(define-syntax (struct/spec stx)
  (syntax-parse stx
    [(_ name:id ([fld:id fld-spec:spec] ...))
     (with-syntax* ([safe-constructor (syntax-property
                                       (format-id stx "make-~a" (syntax-e #'name))
                                       'define/spec-error-name
                                       (syntax-e #'name))]
                    [unsafe-constructor
                     (format-id stx "unsafe-make-~a" (syntax-e #'name))]
                    [predicate
                     (format-id stx "~a?" (syntax-e #'name))]
                    [(name-fld ...)
                     (for/list ([fld (in-syntax #'(fld ...))])
                       (format-id fld "~a-~a"
                                  (syntax-e #'name)
                                  (syntax-e fld)))]
                    [(fld-idx ...) (range (length (syntax->list #'(fld ...))))]
                    [(fld-pat ...) (generate-temporaries #'(fld ...))])
       (syntax/loc stx
         (begin
           (define-values (unsafe-constructor
                           predicate
                           name-fld ...)
             (let ()
               (struct name (fld ...) #:transparent
                 #:constructor-name unsafe-constructor)
               (values unsafe-constructor
                       predicate
                       name-fld ...)))
           (define/spec (safe-constructor fld ...)
             (-> fld-spec ... any)
             (unsafe-constructor fld ...))
           (define-match-expander name
             (λ (stx)
               (syntax-case stx ()
                 [(_ fld-pat ...)
                  (syntax/loc stx (? predicate
                                     (and (app (λ (x) (unsafe-struct*-ref x fld-idx))
                                               fld-pat)
                                          ...)))]))
             (λ (stx)
               (syntax-case stx ()
                 [(_ . args) (syntax/loc stx (safe-constructor . args))]))))))]))

