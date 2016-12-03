; Homework 5 CS 131 

; null-id?
; listdiff is empty iff the pair point to the same object 
(define (null-ld? obj)
	(if (and (pair? obj) (eq? (car obj) (cdr obj)))
		#t
		#f))

; listdiff? 
; obj may or may not be a proper list 
; to be a listdiff, (car obj) and (cdr obj) must referentially point 
; to the same list elements 
(define (listdiff? obj)
	(if (not (pair? obj))
		#f 
		(let listdiffrec ((first (car obj)) (second (cdr obj)))
			(if (eq? first second)
				#t	
				(cond 
					((null? first) (if (null? second)
						#t
						#f))
					((pair? first) (listdiffrec (cdr first) second))
					(else #f))))))

; cons-ld
(define (cons-ld obj listdiff)
	(cons 
		(cons obj (car listdiff)) 
		(cdr listdiff)))

;car-ld 
;assumes listdiff is validly formed and non-empty 
;therefore does no error-checking 
;This eliminates a lot of special cases 
;Notice this fxn will not return the correct answer for an empty listdiff
(define (car-ld listdiff)
	(car (car listdiff)))

;cdr-ld 
;returns modified listdiff with first element removed 
;assumes listdiff is validly formed and non-empty 
;this eliminates checking if first needs to be a pair or not 
(define (cdr-ld listdiff)
	(cons (cdr (car listdiff)) (cdr listdiff)))

;cdr-ld that returns a shallow copy 
(define (cdr-ld-shallow listdiff)
	(let cdr-ld-rec ((first (car listdiff)) (second (cdr listdiff)) (accum '()))
		(if (eq? first second)
			(cons (cdr accum) '())
			(cdr-ld-rec (cdr first) second (append accum (list (car first)))))))

;listdiff 
;rest argument is bound to a list 
(define (listdiff . obj)
	(cons obj '()))

;length-ld 
;assumes well-formed listdiff
;therefore, this function does not worry about the case where 
;traversing (car listdiff) may turn into a single object
(define (length-ld listdiff)
	(let length-ld-rec ((first (car listdiff)) (second (cdr listdiff)) (count 0))
		(if (eq? first second)
			count
			(length-ld-rec (cdr first) second (+ 1 count)))))

;append-ld 
;only append if a listdiff has elements 
(define (append-ld . listdiffs)
	(let append-ld-rec ((ld-list listdiffs) (accum '()))
		(if (null? ld-list)
			(cons accum '())
			(if (null-ld? (car ld-list))
				(append-ld-rec (cdr ld-list) accum)
				(append-ld-rec (cdr ld-list) (append 
												accum 
												(cons
													(car-ld (car ld-list)) 
													(car (cdr-ld-shallow (car ld-list))))))))))

;assq-ld 
(define (assq-ld obj alistdiff)
	(let assq-ld-rec ((target obj) (lp (car alistdiff)))
		(if (null? lp)
			#f
			(if (eq? obj (car (car lp)))
				(car lp)
				(assq-ld-rec target (cdr lp)))))) 

;list->listdiff 
(define (list->listdiff list)
	(cons list '()))

;listdiff->list 
(define (listdiff->list listdiff)
	(if (null-ld? listdiff)
		'()
		(cons 
			(car-ld listdiff) 
			(car (cdr-ld listdiff)))))

;expr-returning
;returns an expression, when evaluated, that will be shallowly
;equivalent to the argument listdiff 
(define (expr-returning listdiff)
	(quasiquote (cons '(unquote (listdiff->list listdiff)) '())))



