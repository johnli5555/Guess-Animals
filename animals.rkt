#lang racket

(provide
 random-animals)

;; This file provides the ability to generate random animals with random
;; attributes that can be used in GuessAnimal
;;
;; To generate random animals put this file in the same folder as GuessAnimal.
;; In order to generate animals, write (random-animals X) where X is an int and
;; will generate X number of random animals

;; This function generates functions that generate specific animals. Those functions
;;   appear directly after this.
(define (random-attributes swims-percent flies-percent angry-percent
                           small-percent medium-percent)
  (local [(define (random-attribute attribute percent base)
            (cond [(> (random 100) percent) base]
                  [else (cons attribute base)]))
          (define binary-attributes
            (random-attribute
             'swims
             swims-percent
             (random-attribute
              'flies
              flies-percent
              (random-attribute
               'angry
               angry-percent
               empty))))
          (define size-percent (random 100))
          (define size-attribute
            (cond [(< size-percent small-percent) 'small]
                  [(< size-percent (+ small-percent medium-percent)) 'medium]
                  [else 'large]))]
    (cons size-attribute binary-attributes)))

;; These functions generate specific animals. The first number determines the percentage
;;   that the animal has the attribute 'swims. The second number determines the percentage
;;   that the animals has the attribute 'flies. The third number determines the percentage
;;   that has the animal has the attribute angry. The fourth and fifth attribute determine
;;   the percentage that animal is small and medium. Note that an animal cannot be both small
;;   and medium and that if it is not small or medium then it is large. (i.e an animal will
;;   have one of the 3: 'small 'medium 'large.
(define (random-goose ignore)
  (cons 'goose (random-attributes 90 90 90 10 20)))
(define (random-squirrel ignore)
  (cons 'squirrel (random-attributes 10 10 90 80 10)))
(define (random-crow ignore)
  (cons 'crow (random-attributes 10 90 90 30 40)))
(define (random-gull ignore)
  (cons 'gull (random-attributes 90 90 90 30 40)))
(define (random-duck ignore)
  (cons 'duck (random-attributes 90 90 10 60 30)))
(define (random-sparrow ignore)
  (cons 'sparrow (random-attributes 10 90 10 80 10)))

;; (random-animals n) generates a random list of n animals with random attributes
;; random-animals: Nat -> (listof (list of Sym))
(define (random-animals n)
  (local [(define animals (list random-goose random-squirrel random-crow
                                random-gull random-duck random-sparrow))
          (define (random-animal ignore)
            ((list-ref animals (random (length animals))) 1))]
    (cond [(<= n 0) empty]
          [else (cons (random-animal 1) (random-animals (sub1 n)))])))
