#lang typed/racket

(require typed/rackunit)


;; ===== DEFINITIONS =====

;; -- ExprC --
(define-type ExprC (U errorC voidC numC idC strC boolC))
(struct errorC ([id : Real]) #:transparent) ;; For inducing errors
(struct voidC ()) ;; Void
(struct numC ([n : Real]) #:transparent) ;; Number
(struct idC ([i : Symbol]) #:transparent) ;; ID
(struct strC ([s : String]) #:transparent) ;; String
(struct boolC ([b : Boolean]) #:transparent) ;; Boolean
(struct appC ([id : Symbol] [args : (Listof ExprC)])) ;; Function call
(struct defC ([id : Symbol] [args : (Listof Symbol)] [body : ExprC])) ;; Function definition

;; -- Value --
(define-type Value (U errV Number Real Boolean Symbol String defC primOp Void))
(struct errV ([id : Real]) #:transparent) ;; For inducing errors
(struct primOp ([op : (-> (Listof Any) Value)])) ;; PrimOp

;; -- Environments & Modules --
(define-type-alias Environment (Listof Module)) ;; Environment
(struct Module ([id : Symbol] [b : (Listof Binding)])) ;; Module
(struct Binding ([id : Symbol] [p : Boolean] [v : Value])) ;; Binding (id, private?, value)


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
(define top-env (list PrimOp-mod IO-mod))

;; ===== HELPER FUNCTIONS =====

;; -- serach-env --
;; takes in an environment and a symbol and returns the corresponding value
(define (search-env [id : Symbol] [env : Environment]) : Value
  (match (symbol->string id)
    [(regexp #rx"([a-z]+).([a-z]+)" (list _ module name)) (search-module
                                                           (string->symbol (cast name String))
                                                           (search-for-module (string->symbol (cast module String)) env))]))

;; -- search-for-module --
;;searches an environment for a module with a given id
(define (search-for-module [id : Symbol] [env : Environment]) : Module
  (cond
    [(empty? env) (error "Elixir: module not found")]
    [(equal? id (Module-id (first env))) (first env)]
    [else (search-for-module id (rest env))]))

;; -- search-module --
;;finds a value in a module given an id
(define (search-module [id : Symbol] [mod : Module]) : Value
  (cond
    [(empty? (Module-b mod)) (error "Elixir: id not found in module")]
    [(equal? id (Binding-id (first (Module-b mod)))) (Binding-v (first (Module-b mod)))]
    [else (search-module id (Module 'DNM (rest (Module-b mod))))]))

;; -- add-module --
;;adds a new module with a given symbol to an environment and returns the environment
(define (add-module [id : Symbol] [env : Environment]) : Environment
  (append env (list (Module id '()))))



;; ========== TEST-CASES ==========

;; ===== TEST-CASES (BUILT-IN) =====

;; -- BIPlus -- test cases
(check-equal? (BIPlus (list 2 3)) 5)
(check-exn exn:fail? (λ () (BIPlus (list 2 's))))
(check-exn exn:fail? (λ () (BIPlus (list 2 3 4))))

;; -- BISub -- test cases
(check-equal? (BISub (list 2 2)) 0)
(check-exn exn:fail? (λ () (BISub (list 2 's))))
(check-exn exn:fail? (λ () (BISub (list 2 3 4))))

;; -- BIMult -- test cases
(check-equal? (BIMult (list 2 3)) 6)
(check-exn exn:fail? (λ () (BIMult (list 2 's))))
(check-exn exn:fail? (λ () (BIMult (list 2 3 4))))

;; -- BIDiv -- test cases
(check-equal? (BIDiv (list 8 4)) 2)
(check-exn exn:fail? (λ () (BIDiv (list 2 's))))
(check-exn exn:fail? (λ () (BIDiv (list 2 3 4))))
(check-exn exn:fail? (λ () (BIDiv (list 2 0))))

;; -- BIConcat -- test cases
(check-equal? (BIConcat (list "Hello" " World!")) "Hello World!")
(check-exn exn:fail? (λ () (BIConcat (list "Hello" 's))))
(check-exn exn:fail? (λ () (BIConcat (list "1" "2" "3"))))

;; -- BIEqual? -- test cases
(check-equal? (BIEqual? (list "Hello" "Hello")) #t)
(check-equal? (BIEqual? (list 10 10)) #t)
(check-equal? (BIEqual? (list "Hello" 10)) #f)
(check-exn exn:fail? (λ () (BIEqual? (list "Hello" 's 10))))

;; -- BI!Equal? -- test cases
(check-equal? (BI!Equal? (list "Hello" "Hello")) #f)
(check-equal? (BI!Equal? (list 10 10)) #f)
(check-equal? (BI!Equal? (list "Hello" 10)) #t)
(check-exn exn:fail? (λ () (BI!Equal? (list "Hello" 's 10))))

;; -- BIPuts -- test cases
(check-not-exn (λ () (BIPuts '("Hello"))))
(check-exn exn:fail? (λ () (BIPuts (list "Hello" 's 10))))

;; -- test cases for search-env --
(check-equal? (search-env 'all.d (list (Module 'all (list (Binding 'd false 3))))) 3)

;; -- test cases for search-module
(check-equal? (search-module 'hello (Module 'id (list (Binding 'no false 1) (Binding 'hello true 2)))) 2)
(check-exn exn:fail? (lambda () (search-module 'hello (Module 'id '()))))

;; -- test cases for search-for-module
(check-equal? (Module-b (search-for-module 'hello (list (Module 'hi (list (Binding 'l false 1)))(Module 'hello '())))) '())
(check-exn exn:fail? (lambda () (search-for-module 'hello '())))

;; -- add-module -- test cases
(check-equal? (Module-id (first (add-module 'Math '()))) 'Math)