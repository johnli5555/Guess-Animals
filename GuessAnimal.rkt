;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname GuessAnimal) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "animals.rkt")
;;
;; Note: For this program to run, it must be in the same folder as animals.rtk
;;
;; ---------------------------------- CODE SUMMARY -------------------------------------
;;
;; This project uses the machine learning algorithm ID3 in order to process
;; a large number of random animals with random character traits using entropy.
;; Then this program builds a decision tree that can determine whether or not
;; a new animal and its attributes fit within the given dataset. the process by which
;; the animals are generated is in animals.rkt
;;
;; -------------------------------- DATA DEFINITIONS -----------------------------------
;;
;; An Example is a (cons Sym (listof Sym))
;; Requires: each attribute in the rest is unique

;; A Histogram is a (listof (list Sym Nat))
;; Requires: A symbol can appear in only one pair.

;; An Augmented Histogram (AH) is a (listof (list Sym Nat Nat))
;; Requires: A symbol can appear in only one triple.

;; An Entropy Association List (EAL) is a (listof (list Sym Num))
;; Requires: A symbol can appear in only one pair.

;; A Decision Tree (DT) is one of:
;; * Bool
;; * (list Sym DT DT)
;;
;;  ------------------------------------------------------------------------------------
;; Functions:
;;
;; (contains-str? Str (listof Str)) consumes a string and a list of 
;;   string and determines if that string exists in that list of strings
;; contains-str?: Str (listof Str) --> Bool
(define (contains-str? s lst)
  (cond [(empty? lst) false]
        [(symbol=? (first lst) s) true]
        [else (contains-str? s (rest lst))]))

;; (collect-attribute examples) consumes a list of examples and
;;   collects all attirbutes an examples might have
;; collect-attribute: (listof Example) --> (listof Sym)
(define (collect-attributes lst)
  (local
    [(define (remove-firsts lst)
       (cond [(empty? lst) empty]
             [else (cons (rest (first lst))
                         (remove-firsts (rest lst)))]))
     (define (append-all lst)
       (cond [(empty? lst) empty]
             [else (append (first lst) (append-all (rest lst)))]))
     (define (find-unique lst result-lst)
       (cond [(empty? lst) result-lst]
             [(contains-str? (first lst) result-lst)
              (find-unique (rest lst) result-lst)]
             [else (find-unique (rest lst)
                                (cons (first lst) result-lst))]))]
    (find-unique (append-all (remove-firsts lst)) empty)))

;; (split-examples examples symbol) Consumes a listof Examples and a
;;   symbol and produce a list of two lists of examples, with the first
;;   containing the examples containing the symbol and the second
;;   containing the examples not containing the symbol
;; split-examples: (listof Example) Sym) --> (list (listof Example)) (listof Example)))
(define (split-examples lst sym)
  (local
    [(define (construct-split lst sym r-lst1 r-lst2)
     (cond [(empty? lst) (list r-lst1 r-lst2)]
           [(contains-str? sym (first lst))
            (construct-split (rest lst) sym
                             (cons (first lst) r-lst1) r-lst2)]
           [else
            (construct-split (rest lst) sym r-lst1
                             (cons (first lst) r-lst2))]))]
    (construct-split lst sym empty empty)))

;;
;; (c)
;;
;; (histogram examples) consumes a list of examples and
;;   produces a list of attribute/count pairs, with each pair
;;   indicating how many times that attribute appears in the examples
;; histogram: (listof Examples) --> Histogram
(define (histogram lst)
  (local
    [(define (remove-firsts lst)
       (cond [(empty? lst) empty]
             [else (cons (rest (first lst))
                         (remove-firsts (rest lst)))]))
     (define (increment-sym sym hist)
              (cond [(empty? hist) empty]
                    [(symbol=? sym (first (first hist)))
                     (cons (list (first (first hist))
                                 (+ (second (first hist)) 1))
                           (increment-sym sym (rest hist)))]
                    [else
                     (cons (first hist)
                           (increment-sym sym (rest hist)))]))
     (define (nested-contains-str? sym lst)
       (cond [(empty? lst) false]
             [(list? (first lst))
              (or (nested-contains-str? sym (first lst))
                  (nested-contains-str? sym (rest lst)))]
             [(equal? (first lst) sym) true]
             [else (nested-contains-str? sym (rest lst))]))
     (define (append-all lst)
       (cond [(empty? lst) empty]
             [else (append (first lst) (append-all (rest lst)))]))
     (define (cons-histogram lst result-lst)
       (cond [(empty? lst) result-lst]
             [(nested-contains-str? (first lst) result-lst)
              (cons-histogram (rest lst)
                              (increment-sym (first lst) result-lst))]
             [else (cons-histogram
                    (rest lst) (cons (list (first lst) 1)
                                     result-lst))]))]
    (cons-histogram (append-all (remove-firsts lst)) empty)))

;; (augment-histogram histogram attributes total) Consumes a Histogram
;;   a list of attributes and a total number of examples and produces
;;   an Augment Histogram
;; augmented-histogram: Histogram (listof Sym) -> AH
;; requires: total must be larger than all numbers in the histogram
(define (augment-histogram hist lst tot)
  (local [(define (nested-contains-str? sym lst)
            (cond [(empty? lst) false]
                  [(list? (first lst))
                   (or (nested-contains-str? sym (first lst))
                       (nested-contains-str? sym (rest lst)))]
                  [(equal? (first lst) sym) true]
                  [else (nested-contains-str? sym (rest lst))]))
          (define (augment sym hist tot)
            (cond [(empty? hist) empty]
                  [(symbol=? sym (first (first hist)))
                   (cons (list (first (first hist))
                               (second (first hist))
                               (- tot (second (first hist))))
                         (augment sym (rest hist) tot))]
                  [else
                     (cons (first hist)
                           (augment sym (rest hist) tot))]))
          (define (add-missing sym hist tot)
            (cons (list sym 0 tot) hist))]
    (cond [(empty? lst) hist]
          [(nested-contains-str? (first lst) hist)
           (augment-histogram (augment (first lst) hist tot)
                              (rest lst) tot)]
          [else
           (augment-histogram (add-missing (first lst) hist tot)
                              (rest lst) tot)])))

;; (entropy positive-counts negative-counts) Consumes two elements from
;;   augnmented histograms and produces their entropy
;; entropy: (list Sym Nat Nat) (list Sym Nat Nat) --> Num
;; requires: The 2 Syms must be identical
(define (entropy pc nc)
  (local [(define a (second pc))
          (define b (second nc))
          (define c (third pc))
          (define d (third nc))
          (define (P n m)
            (cond [(= (+ n m) 0) 0.5] 
                  [else (/ n (+ n m))])) 
          (define (e p)
            (cond [(= p 0) 0] 
                  [else (* (- p) (log p 2))]))]
    (+ (* (P (+ a b) (+ c d)) (+ (e (P a b)) (e (P b a))))
       (* (P (+ c d) (+ a b)) (+ (e (P c d)) (e (P d c))))))) 

;; (entropy-attributes positive negative) consumes two augmented 
;;   histograms and computes the entropy of each attribute, producing
;;   a list of attribute/entropy pairs
;; entropy-attributes: AH AH --> EAL
;; requires: the two AHs must have the same length
;;           the corresponding attributes of the two AH must be the same
(define (entropy-attributes p n)
  (cond [(empty? p) empty]
        [else (cons (list (first (first p))
                          (entropy (first p) (first n)))
                    (entropy-attributes (rest p) (rest n)))]))

;; (best-attribute entropies) consumes a non-empty list of
;;   attribute/entropy pairs and produces the attribute with
;;   the minimum entropy
;; best-attribute: EAL --> Sym
;; requires: EAL cannot be empty
(define (best-attribute eal)
  (local [(define (best-pair eal)
            (cond [(= (length eal) 1) (first eal)]
                  [else (local [(define best (best-pair (rest eal)))] 
                    (cond [(< (second (first eal)) (second best))
                     (first eal)]
                    [else best]))]))]
    (first (best-pair eal))))

;; (build-dt examples label) consumes a list of examples and a label and
;;   produces a decision tree
;; build-dt: (listof Example) Sym --> DT
(define (build-dt lst lab)
  (local [(define attributes (collect-attributes lst))
          (define positive-ex (first (split-examples lst lab)))
          (define negative-ex (second (split-examples lst lab)))]
    (cond [(empty? positive-ex) false]
          [(empty? negative-ex) true]
          [(empty? attributes)
           (cond [(> (length positive-ex) (length negative-ex)) true] 
                 [else false])]
          [else (local
                  [(define (remove-all lst2 sym)
                     (cond [(empty? lst2) empty]
                           [(list? (first lst2))
                            (cons (remove-all (first lst2) sym)
                                  (remove-all (rest lst2) sym))]
                           [(symbol=? (first lst2) sym)
                            (remove-all (rest lst2) sym)]
                           [else
                            (cons (first lst2)
                                  (remove-all (rest lst2) sym))]))
                   (define root-attribute
                          (best-attribute (entropy-attributes
                                           (augment-histogram
                                            (histogram positive-ex)
                                            attributes
                                            (length positive-ex))
                                           (augment-histogram
                                            (histogram negative-ex)
                                            attributes
                                            (length negative-ex))))) 
                   (define with-ra
                     (first (split-examples lst root-attribute)))
                   (define without-ra
                     (second (split-examples lst root-attribute)))
                   (define left-tree
                     (build-dt (remove-all with-ra root-attribute) lab))
                   (define right-tree (build-dt without-ra lab))]
                  (cond [(equal? left-tree right-tree) right-tree]
                        [else
                         (list root-attribute left-tree right-tree)]))])))

;; (train-classifier examples label) Consumes a listof Examples and a
;;   label and produces a predicate that determines wheter or not
;;   a provided example is the label
;; train-classifier: (listof Examples) Sym --> (Example --> Bool) 
(define (train-classifier lst lab)
  (local [(define DT (build-dt lst lab))
          (define (traverse-tree DT ex)
            (cond [(equal? DT true) true]
                  [(equal? DT false) false]
                  [(contains-str? (first DT) ex)
                   (traverse-tree (second DT) ex)]
                  [else (traverse-tree (third DT) ex)]))        
          (define (classifier ex)
                   (traverse-tree DT ex))] 
    classifier))

;; ------------------------------------- TESTS -----------------------------------------

;; The program uses 1000 random animals to build a decision tree for goose
(define goose? (train-classifier (random-animals 1000) 'goose))

;; The program can determine whether or not the following sets of attributes
;; fit with that of a goose

;; This set of attributes fits with the general set of attributes for goose
(check-expect (goose? (list 'large 'angry 'flies 'swims)) true)

;; This set of attributes do not fit with a regular goose
(check-expect (goose? (list 'small 'angry)) false)

;; The program uses 1000 random animals to build a decision tree for squirrel
(define squirrel? (train-classifier (random-animals 1000) 'squirrel))

;; The program can determine whether or not the following sets of attributes
;; fit with that of a squirrel
(check-expect (squirrel? (list 'large 'angry 'flies 'swims)) false)
(check-expect (squirrel? (list 'small 'angry)) true)

;; The program uses 1000 random animals to build a decision tree for crows
(define crow? (train-classifier (random-animals 1000) 'crow))

;; The program can determine whether or not the following sets of attributes
;; fit with that of a crow
(check-expect (crow? (list 'angry 'flies 'medium)) true)

;; ------------------------------------------------------------------------------------
