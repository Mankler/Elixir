#lang typed/racket

(require typed/rackunit)

;; ===== Defintions =====

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
(define-type Value (U errV Number Real Boolean Symbol String defC primOp))
(struct errV ([id : Real]) #:transparent) ;; For inducing errors
(struct primOp ([op : (-> (Listof Any) Value)])) ;; PrimOp

;; -- Environments & Modules --
(define-type-alias Environment (Listof Module)) ;; Environment
(define-type-alias Module (Listof Binding)) ;; Module
(struct Binding ([id : Symbol] [p : Boolean] [v : Value])) ;; Binding (id, private?, value)

