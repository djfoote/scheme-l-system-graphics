;;; Davis Foote
;;; djfoote@berkeley.edu
;;;
;;; lsystem.scm
;;;
;;; A module for Scheme as implemented in the UCB CS 61A Scheme interpreter 
;;; project. 
;;;
;;; Defines Lindenmayer systems and evaluates a system's state.
;;;
;;; Read about L-systems at http://en.wikipedia.org/wiki/Lindenmayer_system

(load "graphic-ops.scm")


;;; Creates an L-system with the given parameters.
;;; An L-system is represented as a function that takes in an integer n and 
;;;	returns the state of the system after n iterations as a list of symbols.
;;; 
;;; start - The system's initial state as a list of symbols.
;;; rules - A list of lists of symbols. Each list's first symbol should
;;;   be the symbol to be replaced, and the remainder of the list should
;;;   be the symbols it should be replaced with.
;;;
;;; return - The L-system function.
(define (lsys start rules)
	
	;;; The returned function that represents the L-system.
	;;;
	;;; n - The number of iterations of the L-system's ruleset to be applied.
	;;;
	;;; return - State of the system after n iterations as a list of symbols.
	(lambda (n) (n-iters start rules n)))

;;; Calculates the state of the system after n iterations. Exists as a separate
;;; function because an L-system can operate as a function with one parameter,
;;; however more are needed for the recursive calls.
;;;
;;; current - The system's current state as a list of symbols.
;;; rules - A list of lists of symbols. Each list's first symbol should
;;;   be the symbol to be replaced, and the remainder of the list should
;;;   be the symbols it should be replaced with.
;;; n - The number of iterations of the L-system's ruleset to be applied.
;;;
;;; return - The state of the system after n iterations as a list of symbols.
(define (n-iters current rules n)
	(if (= n 0) 
		current 
		(n-iters (itersys nil current rules) rules (- n 1))))

;;; Calculates the next iteration of the L-system with given parameters as a
;;; list of symbols.
;;;
;;; prepend - A list of symbols to be prepended the the system's state.
;;;		Necessary for itersys to be tail-recursive. Should be nil for initial
;;;		call to itersys.
;;; current - The system's current state as a list of symbols.
;;; rules - A list of lists of symbols. Each list's first symbol should
;;;   be the symbol to be replaced, and the remainder of the list should
;;;   be the symbols it should be replaced with.
;;;
;;; return - The state of the system after 1 iteration as a list of symbols.
(define (itersys prepend current rules)
	(if (not (null? current)) 
		(begin
			(define replacement (rules-modify (car current) rules))))
	(cond ((null? current) prepend)
				((null? rules) (append prepend current))
				((list? replacement)
					(itersys (append prepend replacement) (cdr current) rules))
				(else 
					(if (null? prepend) 
						(define prepend (list (car current)))
						(define prepend (append prepend (list (car current)))))
					(itersys prepend (cdr current) rules))))


;;; Determines whether there is a rule that replaces a given symbol 
;;; and if there is, returns what it should be replaced with. 
;;;
;;; symbol - The symbol being searched for.
;;; rules - The list of rules (as described above) being searched through for a
;;; 	rule that replaces symbol.
;;;
;;; return - The list of symbols that symbol should be replaced with if a rule 
;;; 	that replaces it is found. #f otherwise.
(define (rules-modify symbol rules)
	(cond ((null? rules) #f)
				((eq? symbol (car (car rules))) (cdr (car rules)))
				(else (rules-modify symbol (cdr rules)))))

(define (a) (color "brown") (pensize (max 1 (- 4 (length stack)))) (fd 3))
(define (b) (color "green") (fd 5))
(define (f) (set! greenlevel (op greenlevel .01))
						(if (> greenlevel .9) (set! op -))
						(if (< greenlevel .6) (set! op +))
						(color3 .5 greenlevel .5)
						(begin_fill)
						(fd 10) (left 90) 
						(fd 10) (left 90)
						(fd 10) (left 90) 
						(fd 10) (left 90)
						(end_fill)
						(fd 10))
(define (u) 
	(color3 0.6568627450980393 0.6882352941176471 0.6215686274509803) (fd 10))
(define (l) (left 25))
(define (r) (right 25))
(define (p) (left 90))
(define (q) (right 90))
(define (x) nil)
(define (y) nil)
(define o open)
(define (c) 
	(right 90)
	(pensize 1) (color "green") (begin_fill) (circle 2) (end_fill) (close))
(define d close)

(define greenlevel .6)
(define op +)

;;; Example case: Algae
(define algae (lsys (list 'a) (list (list 'a 'a 'p) (list 'p 'a))))

;;; Example case: Sierpinski
(define sierpinski 
	(lsys (list 'a) (list (list 'a 'b 'r 'a 'r 'b) (list 'b 'a 'l 'b 'l 'a))))

(define cool1
	(lsys (list 'a 'r 'a 'r 'a) (list (list 'a 'a 'a 'a 'r 'a 'a 'a 'a 'a))))

(define twig
	(lsys (list 'x) 
				(list (list 'x 'a 'l 'o 'o 'x 'd 'r 'x 'd 'r 'a 'o 'r 'a 'x 'd 'l 'x)
							(list 'a 'a 'a))))

(define seaweed
	(lsys (list 'b)
				(list 
					(list 'b 
								'b 'b 'l 'o 'l 'b 'r 'b 'r 'b 'd 'r 'o 'r 'b 'l 'b 'l 'b 'd))))

(define evan
	(lsys (list 'p)
		(list (list 'p 'b 'r 'r 'p 'l 'l 'o 'a 'r 'p 'l 'd)
					(list 'b 'b 'b))))

(define dragon
	(lsys (list 'f 'x)
		(list (list 'x 'x 'p 'y 'f)
					(list 'y 'f 'x 'q 'y))))

(define hilbert
	(lsys (list 'x) 
		(list (list 'x 'p 'y 'u 'q 'x 'u 'x 'q 'u 'y 'p)
					(list 'y 'q 'x 'u 'p 'y 'u 'y 'p 'u 'x 'q))))

(define (draw-background)
	(pu) (setpos -400 -400) (pd)
	(pensize 24)
	(seth 90)
	(color3 0.8 0.83137 0.60784)
	(background 40)
	(pensize 1))

(define (background steps)
	(cond ((> steps 0) 
		(color3 (+ (getr) (/ 0.12941 40)) 
						(+ (getg) (/ 0.10588 40)) 
						(+ (getb) (/ 0.25098 40)))
		(fd 800) (back 800) (left 90) (fd 20) (right 90)
		(background (- steps 1)))))

;;; 
(define (eval-state state)
	(cond ((not (null? state)) 
		((eval (car state))) ; Evaluate the first symbol in the state 
		(eval-state (cdr state)))))

;;; Make width depend on current length of the stack?

(define (draw-trunk len width) 
	(cond ((> len 0) 
					(left 90)
					(back (/ width 2)) 
					(fd width) 
					(back (/ width 2)) 
					(right)
					(fd 0.1)
					(color )
					(draw-trunk (- len 0.1) width ))
		)
	)

(define (circle-silliness rad)
	)

(define (draw)
	(speed 0)
	(draw-background)
	(pu) (setpos 120 60) (seth -30) (pd) (eval-state (dragon 10))
	(pu) (setpos -170 -160) (seth 30) (pd) (eval-state (twig 6)))


;;; Represented (X Y Z)
; (define heading3d (list 0 1 0))

; (define up3d (list 0 0 1))

; (define left3d (list -1 0 0))

; (define (turn3d alpha)
; 	(set! heading3d 
; 		(list (- (* (car heading3d) (cos alpha)) (* (car left3d) (sin alpha))) 
; 			(- (* (cadr heading3d) (cos alpha)) (* (cadr left3d) (sin alpha))) 
; 			(- (* (caddr heading3d) (cos alpha)) (* (caddr left3d) (sin alpha)))))
; 	(set! left3d 
; 		(list (+ (* (car heading3d) (sin alpha)) (* (car left3d) (cos alpha))) 
; 			(+ (* (cadr heading3d) (sin alpha)) (* (cadr left3d) (cos alpha))) 
; 			(+ (* (caddr heading3d) (sin alpha)) (* (caddr left3d) (cos alpha))))))

; (define (pitch3d alpha)
; 	(set! heading3d 
; 		(list (+ (* (car heading3d) (cos alpha)) (* (car up3d) (sin alpha))) 
; 			(+ (* (cadr heading3d) (cos alpha)) (* (cadr up3d) (sin alpha))) 
; 			(+ (* (caddr heading3d) (cos alpha)) (* (caddr up3d) (sin alpha)))))
; 	(set! up3d 
; 		(list (- (* (car up3d) (cos alpha)) (* (car heading3d) (sin alpha))) 
; 			(- (* (cadr up3d) (cos alpha)) (* (cadr heading3d) (sin alpha))) 
;				(- (* (caddr up3d) (cos alpha)) (* (caddr heading3d) (sin alpha))))))

; (define (roll3d alpha)
; 	(set! left3d 
; 		(list (+ (* (car left3d) (cos alpha)) (* (car up3d) (sin alpha))) 
; 			(+ (* (cadr left3d) (cos alpha)) (* (cadr up3d) (sin alpha))) 
; 			(+ (* (caddr left3d) (cos alpha)) (* (caddr up3d) (sin alpha)))))
; 	(set! up3d 
; 		(list (- (* (car up3d) (cos alpha)) (* (car left3d) (sin alpha))) 
; 			(- (* (cadr up3d) (cos alpha)) (* (cadr left3d) (sin alpha))) 
; 			(- (* (caddr up3d) (cos alpha)) (* (caddr left3d) (sin alpha))))))




