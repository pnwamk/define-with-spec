#lang racket/base

(require "main.rkt")


(define/spec (plus n m)
  (-> number? number? number?)
  (+ n m))

(plus 1 2) ;; ==> 3

;; (plus 1 "2")
;; produces the error:
;; plus: /Users/pnwamk/Repos/define-with-spec/examples.rkt:12:0
;;  2nd argument to plus failed predicate!
;;  Expected: number?
;;  Given: "2"


;; (map plus
;;      '(1 2 3 "4")
;;      '(5 6 7 8))
;; produces the error:
;; plus: /Users/pnwamk/Repos/define-with-spec/examples.rkt:60:5
;; 1st argument to plus failed predicate!
;; Expected: number?
;; Given: "4"

(define/spec (lookup env x)
  (-> (listof (list symbol? any))
      symbol?
      (either (list symbol? any) #f))
  (assoc x env)) 

(define Γ '((x Int) (y Bool) (z String)))

(lookup Γ 'x) ;; ==> '(x Int)
(lookup Γ 'a) ;; ==> #f

;; (lookup)
;; produces the error:
;; lookup: Expected 2 arguments, given 0 in: (lookup)

;; (lookup Γ 42)
;; produces the error:
;; lookup: .../examples.rkt:21:0
;;  2nd argument to lookup failed predicate!
;;  Expected: symbol?
;;  Given: 42

(define/spec (vec-ref vec idx)
  (-> vector?
      (both number? exact-integer? (except negative?))
      any)
  (vector-ref vec idx))

(vec-ref (vector 'a 'b 'c) 1) ;; => 'b

;; (vec-ref (vector 'a 'b 'c) -1)
;; produces the error:
;; vec-ref: .../examples.rkt:36:0
;;  2nd argument to vec-ref failed predicate!
;;  Expected: (both number? exact-integer? (except negative?))
;;  Given: -1


(define/spec (make-posn x y)
  (-> number? number? (pair number? number?))
  (list x y))

;; (make-posn 1 2)
;; produces the error:
;; make-posn: /Users/pnwamk/Repos/define-with-spec/examples.rkt:48:0
;;  Returned invalid result!
;;  Promised: (pair number? number?)
;;  Returned: '(1 2)

