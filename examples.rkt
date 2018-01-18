#lang racket/base

(require define-with-spec
         rackunit racket/string
         (for-syntax racket/base syntax/parse racket/string))
; WARNING - check-exn tests include current line numbers -- adding newlines may 
; break tests, i.e. add new content to the END of the file whenever possible
(define ((failure-with-msgs msgs) e)
  (and (exn:fail? e)
       (for/and ([msg (in-list msgs)])
         (string-contains? (exn-message e) msg))))


(define/spec (plus n m)
  (-> number? number? number?)
  (+ n m))

(check-equal? (plus 1 2) 3)

(check-exn
 (failure-with-msgs '(".rkt:25:7"
                      "2nd argument to plus failed predicate!"
                      "Expected: number?"
                      "Given: \"2\""))
 (λ () (plus 1 "2")))


(check-exn
 (failure-with-msgs '(".rkt:33:12"
                      "1st argument to plus failed predicate!"
                      "Expected: number?"
                      "Given: \"4\""))
 (λ () (map plus
            '(1 2 3 "4")
            '(5 6 7 8))))

(define/spec (lookup env x)
  (-> (listof (list symbol? any))
      symbol?
      (either (list symbol? any) #f))
  (assoc x env))

(define Γ '((x Int) (y Bool) (z String)))

(check-equal? (lookup Γ 'x) '(x Int))
(check-equal? (lookup Γ 'a) #f)


(check-exn
 (failure-with-msgs '(".rkt:54:7"
                      "2nd argument to lookup failed predicate!"
                      "Expected: symbol?"
                      "Given: 42"))
 (λ () (lookup Γ 42)))

(define/spec (vec-ref vec idx)
  (-> vector?
      (both number? exact-integer? (except negative?))
      any)
  (vector-ref vec idx))

(check-equal?
 (vec-ref (vector 'a 'b 'c) 1)
 'b)

(check-exn
 (failure-with-msgs '(".rkt:71:7"
                      "2nd argument to vec-ref failed predicate!"
                      "Expected: (both number? exact-integer? (except negative?))"
                      "Given: -1"))
 (λ () (vec-ref (vector 'a 'b 'c) -1)))


(define/spec (make-posn x y)
  (-> number? number? (cons number? number?))
  (list x y))

(check-exn
 (failure-with-msgs '(".rkt:84:7"
                      "make-posn returned invalid result!"
                      "Promised: (cons number? number?)"
                      "Returned: '(1 2)"
                      "Argument list: '(1 2)"))
 (λ () (make-posn 1 2)))

(struct/spec 3dposn ([x real?] [y real?] [z real?]))

(check-exn
 (failure-with-msgs '(".rkt:93:7"
                      "3rd argument to 3dposn failed predicate!"
                      "Expected: real?"
                      "Given: 'c"))
 (λ () (3dposn 1 2 'c)))

(check-exn
 (failure-with-msgs '(".rkt:100:7"
                      "2nd argument to 3dposn failed predicate!"
                      "Expected: real?"
                      "Given: 'b"))
 (λ () (3dposn 1 'b 3)))

(check-exn
 (failure-with-msgs '(".rkt:107:7"
                      "1st argument to 3dposn failed predicate!"
                      "Expected: real?"
                      "Given: 'a"))
 (λ () (3dposn 'a 2 3)))


;; tests for syntax-errors
(define-syntax (check-phase-1-exn-msgs stx)
  (syntax-parse stx
    [(_ (msg:str ...) body)
     (with-handlers ([(λ (e)
                        (and (exn:fail:syntax? e)
                             (for/and ([msg (in-list (syntax->datum #'(msg ...)))])
                               (string-contains? (exn-message e) msg))))
                      void])
       (local-expand #'body 'module '()))
     (syntax/loc stx (check-equal? #t #t))]))

(check-phase-1-exn-msgs
 ("Expected 2 arguments, given 0")
 (lookup))

(check-phase-1-exn-msgs
 ("Expected 2 arguments, given 1")
 (lookup '()))

(check-phase-1-exn-msgs
 ("Expected 2 argument specs for foo, given 1")
 (define/spec (foo bar baz)
   (-> integer? integer?)
   'hi))

(check-phase-1-exn-msgs
 ("bad syntax")
 (struct/spec test-struct (x [x number?])))

(check-phase-1-exn-msgs
 ("expected more terms")
 (struct/spec test-struct ([x] [x number?])))

(check-phase-1-exn-msgs
 ("Duplicate field names are not allowed; x appeared more than once")
 (struct/spec test-struct ([x number?] [x number?])))
