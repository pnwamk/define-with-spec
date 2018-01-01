#lang racket/base

(require racket/match
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     racket/sequence))

(provide define/spec -> either both except pair listof any)
(provide (for-syntax define/spec-enforcement?))

(define-for-syntax define/spec-enforcement? (make-parameter #t))

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
(define-syntax (pair stx)
  (raise-syntax-error 'pair "only valid in define/spec specification" stx))
(define-syntax (listof stx)
  (raise-syntax-error 'listof "only valid in define/spec specification" stx))
(define-syntax (any stx)
  (raise-syntax-error 'any "only valid in define/spec specification" stx))

(define-syntax (define/spec stx)
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
    (pattern ((~literal pair) a:spec d:spec)
             #:attr pred #'(λ (x) (and (pair? x) (a.pred (car x)) (d.pred (cdr x)))))
    (pattern ((~literal list) positional:spec ...)
             #:attr pred #'(λ (x) (match x [(list (? positional.pred) ...) #t] [_ #f])))
    (pattern ((~literal listof) elem:spec)
             #:attr pred #'(λ (x) (and (list? x) (andmap elem.pred x))))
    (pattern (~or val:boolean (~and val ((~literal quote) _:id)))
             #:attr pred #'(λ (x) (eqv? x val))))
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
       [(define/spec-enforcement?)
        (with-syntax ([(provided-arg ...)
                       (generate-temporaries #'(arg ...))]
                      [fun-name (generate-temporary #'name)])
          (quasisyntax/loc stx
            (begin
              (define (fun-name arg ...) . body)
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
                                 (error 'name
                                        "~a:~a:~a\n ~a~a argument to ~a failed predicate!\n ~a\n ~a"
                                        src
                                        line
                                        col
                                        i
                                        (ending i)
                                        'name
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
                               (error 'name "~a:~a:~a\n Returned invalid result!\n ~a\n ~a"
                                      src
                                      line
                                      col
                                      (format "Promised: ~a" 'rng-spec)
                                      (format "Returned: ~v" result))))])
                       (syntax/loc usage-stx
                         (let ([arg provided-arg] ...)
                           (unless (spec a)
                             arg-error)
                           (... ...)
                           (define result (fun-name arg ...))
                           (unless (rng-spec.pred result)
                             range-error)
                           result)))]
                    [usage
                     (identifier? #'usage)
                     (with-syntax ([lambda-body
                                    (syntax/loc #'usage
                                      (name arg ...))])
                       #'(λ (arg ...) lambda-body))]))))))]
       [else
        (syntax/loc stx
          (define (name arg ...) . body))])]))

