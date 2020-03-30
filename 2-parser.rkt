#lang plai-typed

;; create a datatype
(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

;; a function convert s-expressions into instances of
;; this datatype
(define (parse [s : s-expression])
  (cond
      [(s-exp-number? s) (numC (s-exp->number s))]
      [(s-exp-list? s)
        (let ([sl (s-exp->list s)])
            (case (s-exp->symbol (first sl))
               [(+) (plusC (parse (second sl)) (parse (third sl)))]
               [(*) (multC (parse (second sl)) (parse (third sl)))]
               [else (error 'parse "invalid list input")]))] ; end s-exp list

        [else (error 'parse "invalid input")]))

; test-case: (parse '(+ (* 1 2) (+ 2 3)))
; if (parse (+ (* 1 2) (+ 2 3)))
; then typecheck failed: s-expression vs. number in