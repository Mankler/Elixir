#lang typed/racket

(require typed/rackunit)


;; ===== DEFINITIONS =====

;; -- ExprC --
(define-type ExprC (U errorC numC idC strC boolC appC defC defModC))
(struct errorC ([id : Real]) #:transparent) ;; For inducing errors
(struct numC ([n : Real]) #:transparent) ;; Number
(struct idC ([i : Symbol]) #:transparent) ;; ID
(struct strC ([s : String]) #:transparent) ;; String
(struct boolC ([b : Boolean]) #:transparent) ;; Boolean
(struct appC ([id : Symbol] [args : (Listof ExprC)]) #:transparent) ;; Function call
(struct defC ([id : Symbol] [args : (Listof Symbol)] [body : (Listof ExprC)]) #:transparent) ;; Function definition
(struct defModC ([id : Symbol] [exps : (Listof ExprC)]) #:transparent) ;; Define Module

;; -- Value --
(define-type Value (U errV Number Real Boolean Symbol String defC primOp Void (Listof Value)))
(struct errV ([id : Real]) #:transparent) ;; For inducing errors
(struct primOp ([op : (-> (Listof Any) Value)])) ;; PrimOp

;; -- Environments & Modules --
(define-type-alias Environment (Listof Module)) ;; Environment
(struct Module ([id : Symbol] [b : (Listof Binding)]) #:transparent) ;; Module
(struct Binding ([id : Symbol] [p : Boolean] [v : Value]) #:transparent) ;; Binding (id, private?, value)


;; ===== BUILT-IN FUNCTIONS =====

;; -- BIPlus --
;; Built-in addition function for two numbers
(define (BIPlus [nums : (Listof Any)]) : Value
  (match nums
    [(list (? real? a) (? real? b)) (+ a b)]
    [else (error "Elixir: '+' can only be performed on two numbers")]))

;; -- BISub --
;; Built-in subtraction function for two numbers
(define (BISub [nums : (Listof Any)]) : Value
  (match nums
    [(list (? real? a) (? real? b)) (- a b)]
    [else (error "Elixir: '-' can only be performed on two numbers")]))

;; -- BIMult --
;; Built-in multiplication function for two numbers
(define (BIMult [nums : (Listof Any)]) : Value
  (match nums
    [(list (? real? a) (? real? b)) (* a b)]
    [else (error "Elixir: '*' can only be performed on two numbers")]))

;; -- BIDiv --
;; Built-in division function for two numbers
(define (BIDiv [nums : (Listof Any)]) : Value
  (match nums
    [(list (? real? a) (? real? b))
     (if (not (equal? b 0)) (/ a b) (error "TULI: Division by zero"))]
    [else (error "Elixir: '/' can only be performed on two numbers")]))

;; -- BIConcat --
;; Built-in concatenate function for two strings
(define (BIConcat [strings : (Listof Any)]) : Value
  (match strings
    [(list (? string? a) (? string? b)) (string-append a b)]
    [else (error "Elixir: '<>' can only be performed on two strings")]))

;; -- BIEqual? --
;; Built-in equal? function
(define (BIEqual? [args : (Listof Any)]) : Value
  (match args
    [(list a b) (equal? a b)]
    [else (error "Elixir: '==' can only be performed on two arguments")]))

;; -- BI!Equal? --
;; Built-in not-equal? function
(define (BI!Equal? [args : (Listof Any)]) : Value
  (match args
    [(list a b) (not (equal? a b))]
    [else (error "Elixir: '!=' can only be performed on two arguments")]))

;; -- BIPuts --
;; Built-in print function
(define (BIPuts [exps : (Listof Any)]) : Value
  (match exps
    [(list a) (print a)]
    [else (error "Elixir: 'IO.puts' requires 1 argument")]))


;; ===== TOP-ENV =====

;; -- IO-mod --
(define IO-mod (Module 'IO [list
                            (Binding 'puts #f (primOp BIPuts))]))

;; -- PrimOp-mod --
;; Module containing primOps
(define PrimOp-mod (Module 'PrimOp [list
                                    (Binding '+ #f (primOp BIPlus))
                                    (Binding '- #f (primOp BISub))
                                    (Binding '* #f (primOp BIMult))
                                    (Binding '/ #f (primOp BIDiv))
                                    (Binding '<> #f (primOp BIConcat))
                                    (Binding '== #f (primOp BIEqual?))
                                    (Binding '!= #f (primOp BI!Equal?))]))

;; -- top-env --
;; Top environment containing in-built modules
(define top-env (list
                 (Module 'top-mod (list))
                 PrimOp-mod
                 IO-mod))


;; ===== INTERP =====

;; -- loop-interp --
;; loops through interp over a given list of exprC
(define (loop-interp [eS : (Listof ExprC)] [env : Environment] [curMod : Symbol]) : (Listof Value)
  (map (λ (exp) (interp (cast exp ExprC) env curMod)) eS))

;; -- interp --
;; returns a value after interpreting an expression in an environment
(define (interp [e : ExprC] [env : Environment] [curMod : Symbol]) : Value
  (match e
    [(numC n) n]
    [(strC s) s]
    [(boolC b) b]
    [(idC i) (if (id-exists? (mod-id-concat curMod i) env)
                 (search-env (mod-id-concat curMod i) env)
                 (error "Elixir: Undefined variable"))]
    [(defModC id exps) (loop-interp exps (add-module id env) id)]
    ;;[(? defC? def) (add-to-module def env curMod)] ;; <- NOT IMPLEMENTED
    [(appC id args) (evaluate (interp (idC id) env curMod) (map (λ (arg) (interp (cast arg ExprC) env curMod)) args))]
    [else (error "Elixir: Failure to parse")]))


;; ===== ENVIRONMENT & MODULE MANIPULATION =====
  
;; -- search-env --
;; takes in an environment and a symbol and returns the corresponding value
(define (search-env [id : Symbol] [env : Environment]) : Value
  (match (symbol->string id)
    [(regexp #rx"([a-z]+).([a-z]+)" (list _ module name)) (search-module
                                                           (string->symbol (cast name String))
                                                           (search-for-module (string->symbol (cast module String)) env))]))

;; -- search-for-module --
;; searches an environment for a module with a given id
(define (search-for-module [id : Symbol] [env : Environment]) : Module
  (cond
    [(empty? env) (error "Elixir: module not found")]
    [(equal? id (Module-id (first env))) (first env)]
    [else (search-for-module id (rest env))]))

;; -- search-module --
;; finds a value in a module given an id
(define (search-module [id : Symbol] [mod : Module]) : Value
  (cond
    [(empty? (Module-b mod)) (error "Elixir: id not found in module")]
    [(equal? id (Binding-id (first (Module-b mod)))) (Binding-v (first (Module-b mod)))]
    [else (search-module id (Module 'DNM (rest (Module-b mod))))]))

;; -- add-module --
;; adds a new module with a given symbol to an environment and returns the environment
(define (add-module [id : Symbol] [env : Environment]) : Environment
  (append env (list (Module id '()))))

;; -- add-to-module --
;; adds a new value to a module (NEEDS TO BE IMPLEMENTED)
;;(define (add-to-module [value : Value] [env : Environment] [mod : Symbol]) : Void
  ;;)

;; -- id-exists? --
;; confirms if an id exists within the environment
;; NOT COMPLETE (DON'T PASS NONEXISTENT IDS)
(define (id-exists? [i : Symbol] [env : Environment]) : Boolean
  #t)

;; ===== EVALUATE =====

;; -- evaluate --
;; evaluates a list of arguments within a function
(define (evaluate [fun : Value] [args : (Listof Value)]) : Value
  (match fun
    [(primOp p) (p args)]
    ;;[(defC if args body)] ;;<- not implemented; I'm thinking substitution?
    [else (error "Elixir: Failure to evaluate")]))

;; ===== HELPER =====

;; -- mod-id-concat --
;; concatenates two ids to facilitate environment searching
(define (mod-id-concat [mName : Symbol] [idName : Symbol]) : Symbol
  (cast (string->symbol
         (string-append
          (cast (symbol->string mName) String)
          (string-append "." (cast (symbol->string idName) String)))) Symbol))

;; ========== TEST-CASES ==========

;; ===== TEST-CASES (BUILT-IN) =====

;; -- BIPlus -- 
(check-equal? (BIPlus (list 2 3)) 5)
(check-exn exn:fail? (λ () (BIPlus (list 2 's))))
(check-exn exn:fail? (λ () (BIPlus (list 2 3 4))))

;; -- BISub --
(check-equal? (BISub (list 2 2)) 0)
(check-exn exn:fail? (λ () (BISub (list 2 's))))
(check-exn exn:fail? (λ () (BISub (list 2 3 4))))

;; -- BIMult --
(check-equal? (BIMult (list 2 3)) 6)
(check-exn exn:fail? (λ () (BIMult (list 2 's))))
(check-exn exn:fail? (λ () (BIMult (list 2 3 4))))

;; -- BIDiv --
(check-equal? (BIDiv (list 8 4)) 2)
(check-exn exn:fail? (λ () (BIDiv (list 2 's))))
(check-exn exn:fail? (λ () (BIDiv (list 2 3 4))))
(check-exn exn:fail? (λ () (BIDiv (list 2 0))))

;; -- BIConcat --
(check-equal? (BIConcat (list "Hello" " World!")) "Hello World!")
(check-exn exn:fail? (λ () (BIConcat (list "Hello" 's))))
(check-exn exn:fail? (λ () (BIConcat (list "1" "2" "3"))))

;; -- BIEqual? --
(check-equal? (BIEqual? (list "Hello" "Hello")) #t)
(check-equal? (BIEqual? (list 10 10)) #t)
(check-equal? (BIEqual? (list "Hello" 10)) #f)
(check-exn exn:fail? (λ () (BIEqual? (list "Hello" 's 10))))

;; -- BI!Equal? --
(check-equal? (BI!Equal? (list "Hello" "Hello")) #f)
(check-equal? (BI!Equal? (list 10 10)) #f)
(check-equal? (BI!Equal? (list "Hello" 10)) #t)
(check-exn exn:fail? (λ () (BI!Equal? (list "Hello" 's 10))))

;; -- BIPuts --
(check-not-exn (λ () (BIPuts '("Hello"))))
(check-exn exn:fail? (λ () (BIPuts (list "Hello" 's 10))))

;; ===== TEST-CASES (INTERP) =====

;; -- loop-interp --
(check-equal? (loop-interp (list (numC 10) (numC 15)) top-env 'top-mod) (list 10 15))

;; -- interp --
(check-equal? (interp (numC 10) top-env 'top-mod) 10)
(check-equal? (interp (strC "HI") top-env 'top-mod) "HI")
(check-equal? (interp (boolC #t) top-env 'top-mod) #t)
(check-equal? (interp (defModC 'test-mod (list (numC 10) (numC 15))) top-env 'top-mod) (list 10 15))
;;(check-equal? (interp (idC '+) top-env 'PrimOp) (primOp BIPlus)) <-doesn't work; something wrong with search-for-module
(check-exn exn:fail? (λ () (interp (idC 's) top-env 'top-mod)))

;; ===== TEST-CASES (ENVIRONMENT & MODULE MANIPULATION) =====
  
;; -- search-env --
(check-equal? (search-env 'all.d (list (Module 'all (list (Binding 'd false 3))))) 3)

;; -- search-module --
(check-equal? (search-module 'hello (Module 'id (list (Binding 'no false 1) (Binding 'hello true 2)))) 2)
(check-exn exn:fail? (lambda () (search-module 'hello (Module 'id '()))))

;; -- search-for-module --
(check-equal? (Module-b (search-for-module 'hello (list (Module 'hi (list (Binding 'l false 1)))(Module 'hello '())))) '())
(check-exn exn:fail? (lambda () (search-for-module 'hello '())))

;; -- add-module --
(check-equal? (add-module 'new (list)) (list (Module 'new (list))))

;; -- id-exists --
(check-equal? (id-exists? 's top-env) #t) ;;FAKE
;;(check-equal? (id-exists? 's top-env) #f) ;;REAL
;;(check-equal? (id-exists? '+ top-evn) #t) ;;REAL

;; ===== TEST-CASES (EVALUATE) =====

;; -- evaluate --
(check-equal? (evaluate (primOp BIMult) (list 10 5)) 50)
(check-exn exn:fail? (λ () (evaluate 10 (list 10))))

;; ===== TEST-CASES (HELPER) =====

;; -- mod-id-concat --
(check-equal? (mod-id-concat 'top-mod 'fun) 'top-mod.fun)