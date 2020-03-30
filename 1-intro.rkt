#lang plai-typed

;; Define Data type
(define-type MisspelledAnimal
  [caml (humps : number)]
  [yacc (height : number)])


(define (good? [ma : MisspelledAnimal]) : boolean
    (cond
        [(caml? ma) (>= (caml-humps ma) 2)]
        [(yacc? ma) (> (yacc-height ma) 2.1)]
      ))