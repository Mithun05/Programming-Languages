;;-*- mode: scheme; -*-
;; :set filetype=scheme

;;Return a 2-element list containing the 2 roots of the quadratic
;;equation a*x^2 + b*x + c = 0 using the classical formula for the
;;roots of a quadratic equation.  The first element in the returned list
;;should use the positive square-root of the discriminant, the second
;;element should use the negative square-root of the discriminant.
(define (quadratic-roots a b c)		     		 	; function call 	
	    (if (zero? a)  		     		 	; check for a null
	        (error "Invalid parameters") 		 	; error message for invalid parameters	 	
	        (let ((d (- (* b b) (* 4 a c))))         	; d is discriminant value
	        (let ((f-ratio-1 (/ b a -2)))            	; f-exp-1 is first  fraction value present in formula
                (let ((f-ratio-2 (/ (sqrt d) a 2)))      	; f-exp-2 is second fraction value present in formula	
	    	(cond			     		 	; conditional check
	    		((= d 0) (list f-ratio-1 f-ratio-1))    ; d is zero means both roots are same
	    		(else (list (+ f-ratio-1 f-ratio-2) (- f-ratio-1 f-ratio-2))))))))) ; display roots both real and imaginary if d > 0 and d < 0 	    

; function end



;;Return the list resulting by multiplying each element of `list` by `x`.
(define (mul-list list x)					; function call
  	(if (null? list) 					; check for empty list
      	     '()  						; return empty list
	      (if (number? (car list))				; check for numbers only	
  	      (cons (* x (car list)) (mul-list (cdr list) x))   ; display final list of multiplication
		    (error "Required Numbers"))))  		; error message for not proper list

;function end


;;Given a proper-list list of proper-lists, return the sum of the
;;lengths of all the contained lists.
(define (sum-lengths list)					; function call
	(if (null? list)  					; check for empty list
	    0	  						; return empty
	    (+ (length (car list)) (sum-lengths (cdr list)))))  ; display final result

;function end


;;Evaluate polynomial with list of coefficients coeffs (highest-order
;;first) at x.  The computation should reflect the traditional
;;representation of the polynomial.
(define (poly-eval coeffs x)				       ; function call
  	(if (null? coeffs) 				       ; check empty coeffs	
	0						       ; return empty	
    	(+ (* (expt x (- (length coeffs) 1) ) (car coeffs)) (poly-eval (cdr coeffs) x)))) ; calculate coeffs*x^(x-1) element

;function end

;;Evaluate polynomial with list of coefficients coeffs (highest-order
;;first) at x using Horner's method.
(define (poly-eval-horner coeffs x)							; function call
	(letrec	([poly-eval-horner-rec (lambda (coeffs x)  				; let recusrive call
		 (if (null? coeffs)   							; empty list check
		 0 		     							; return 0
		 (+ (car coeffs) (* x (poly-eval-horner-rec(cdr coeffs) x)))))]) 	; used horner's formula synthetic division/inner nested formula
		 (poly-eval-horner-rec (reverse coeffs) x)))				; tail recursion call

;function end

;;Return count of occurrences equal? to x in exp(define (count-occurrences lst item)
(define (count-occurrences exp x)							; function call
  	(cond										; conditional check
    	((equal? exp x) 1) 								; check for list exp against given expression
        ((not (pair? exp)) 0)								; check for pair not equal	
        (else (+ (count-occurrences (car exp) x) (count-occurrences (cdr exp) x)))))    ; recursive call


; function end


;;Return result of evaluating arith expression over Scheme numbers
;;with fully parenthesized prefix binary operators 'add, 'sub, 'mul
;;and 'div.

(define (eval-arith exp)					   	; function call
  (cond								    	; conditional check
    ((number? exp) exp)   					   	; check for number in an exp 
    (else
    	(let ((operator-name (car exp))  			        ; store exp name in variable
        (operand-1 (eval-arith (cadr exp))) 			   	; store first operand in variable - rec call
        (operand-2 (eval-arith (caddr exp)))) 			   	; store second operand in variable - rec call
  	      (cond							; conditional check
              ((equal? operator-name 'add) (+ operand-1 operand-2)) 	; check for add and perform sum
              ((equal? operator-name 'sub) (- operand-1 operand-2)) 	; check for sub and perform minus
              ((equal? operator-name 'mul) (* operand-1 operand-2)) 	; check for mul and perform star
              ((equal? operator-name 'div) (/ operand-1 operand-2)) 	; check for div and perform div
              (else (error "Invalid Expression:" exp)))))))         	; else give error message invalid expression 

;function end

;;Given a proper-list list of proper-lists, return sum of lengths of
;;all the contained lists.  Must be tail-recursive.
(define (sum-lengths-tr list)							; function call
  	(letrec ((sum-lengths-letrec (lambda (list acc) 			; tail recursion
             (if (null? list)      						; empty list check
             acc								; return valueSoFar i.e. accumulator result
	     (sum-lengths-letrec (cdr list) (+ acc (length (car list)))))))) 	; display final result
	     (sum-lengths-letrec list 0)))   					; tail function call

;function end


;;Evaluate polynomial with list of coefficients coeffs (highest-order
;;first) at x.  Must be tail-recursive.
(define (poly-eval-tr coeffs x)									; function call
	(letrec ((poly-eval-tr-letrec (lambda (coeffs x valueSoFar) 				; tail recursive call
	      (if (null? coeffs)  								; check empty coeffs
	          valueSoFar      								; return valuesofar i.e. accumulator result				
	          (poly-eval-tr-letrec (cdr coeffs) x (+ (* valueSoFar x) (car coeffs)))))))    ; display final result
	          (if (null? coeffs)  								; check for empty list
		  0           									; return empty 
	          (poly-eval-tr-letrec coeffs x 0)))) 						; tail recursive call

;function end

	
;;Return the list resulting by multiplying each element of `list` by `x`.
;;Cannot use recursion, can use one or more of `map`, `foldl`, or `foldr`.
(define (mul-list-2 list x)		; function end						
  	(map (lambda (y)		; used map
		(if (null? y)		; list empty check
		'() 	        	; return empty	
		 (* y x))) list)) 	; iterate over the list element

;function end


;;Given a proper-list list of proper-lists, return the sum of the
;;lengths of all the contained lists.  Cannot use recursion, can use
;;one or more of `map`, `foldl`, or `foldr`.
(define (sum-lengths-2 list)		 ; function call
 	(foldl + 0 (map length list)))   ; apply map on list length using foldl with addition(+) procedure and initial value zero

;function end
