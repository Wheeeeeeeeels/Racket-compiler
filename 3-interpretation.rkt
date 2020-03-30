#lang plai-typed
;; By wheels
;; A First Look at Interpretation
;; A First Taste of Desugaring
; A representation for numbers and arbitrarily netstable addition and multiplication

(define-type ArithC
   [numC (n : number)]
   [plusC (l : ArithC) (r : ArithC)]
   [multC (l : ArithC) (r : ArithC)])

; Define A new datatype to reflect out intended surface syntax terms
(define-type ArithS
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [bminusS(l : ArithS) (r : ArithS)]
  [uminusS(e : ArithS)] ; minus
  [multC (l : ArithS) (r : ArithS)])


; writting an Interpreter
(define (interp [a : ArithC]):number
  (type-case ArithC a)
     [numC (n) n]
     [plusC (l r) (+ interp l)(interp r)]
     [multC (l r) (* interp l)(interp r)]
  )

; writting the obvious part of desugar
<desugar> ::=
(define (desugar [as : ArithS]) : ArithC
   (type-case ArithS as
     [numS (n) (numC n)]
     [plusS (l r) (plusC (desugar l)
                        (desugar r))]
     [multS (l r) (multC (desugar l)
                         (desugar r))]

  <bminusS-case>))

<bminusS-case> ::=
[bminuS (l r) (plusC (desugar l) (multC(numC -1) (desugar r)))]

