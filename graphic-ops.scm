;;; Davis Foote
;;; djfoote@berkeley.edu
;;; 
;;; graphic-ops.scm
;;; 
;;; A module for Scheme as implemented in the UCB CS 61A Scheme interpreter 
;;; project.
;;;
;;; When an L-system state is evaluated, all symbols should evaluate to some 
;;; procedure. This file offers some default procedures for drawing L-systems
;;; with turtle graphics.

;;; elements of stack are of the form ((x-coordinate . y-coordinate) . heading)

(define stack nil)

(define (push x) 
	(set! stack (cons x stack)) 
	stack)
(define (pop)
	(define return (car stack)) 
	(set! stack (cdr stack)) 
	return)

(define (open) 
	(push (cons (getpos) (geth))))
(define (close)
	(define new (pop))
	(pu) (setpos (car (car new)) (cdr (car new))) (pd)
	(seth (cdr new)))