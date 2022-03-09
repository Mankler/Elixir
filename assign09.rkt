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
(struct appC ([name : Symbol] [args : (Listof ExprC)])) ;; Function call
(struct defC ([name : Symbol] [args : (Listof Symbol)] [body : ExprC])) ;; Function definition
;;(struct defPC)

;; -- Value --
(define-type Value (U errV Number Real Boolean Symbol String primOp))
(struct errV ([id : Real]) #:transparent) ;; For inducing errors
(struct primOp ([op : (-> (Listof Any) Value)])) ;; PrimOp