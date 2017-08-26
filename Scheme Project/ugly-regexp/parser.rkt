#!/usr/bin/env racket
;;-*- mode: scheme; -*-
;; :set filetype=scheme

#lang racket

(require "errors.rkt")
(require "scanner.rkt")

;;;************************* Top-Level Globals *************************

;;global var which contains lookahead token
(define *lookahead* #f)

;;global var which specifies function which reads next token from scanner
(define *next-token* #f)

;;;****************************** Parser *******************************

;;Top-level parse function.
;;Parse ugly regexp from line and output translation in standard syntax.
;;If error, catch syntax-error exception, print message and recover.
(define (parse)
  (unless (equal? '<EOF> (token-kind *lookahead*))
	  (with-handlers
	    ([exn:fail:user?
	      (lambda (ex)
		(do-error (exn-message ex))
		(recover-from-error)
		(advance))])
	    (let ([out (ugly-regexp)])
	      (when (check? '<NL>) (displayln out))
	      (match '<NL>)))
	  (parse)))

;   main context free grammer CFG 
;   ugly-regexp -> term regexp-rest
;   regexp-rest -> . term regexp-rest | empty	
;   term -> factor term-rest
;   term-rest -> + factor term-rest | empty
;   factor -> (ugly-regexp) | * factor | chars(t)
;   t -> ,t | t 
;   start of the expression symbol
;   ugly-regexp -> term regexp-rest

(define (ugly-regexp)
  (let ([t (term)])					; store term function's return value
    (regexp-rest t)))					; call regexp-rest passing t as parameter 

; regexp-rest -> . term regexp-rest | empty 

(define (regexp-rest t)
  (if (check? 'CHAR ".")      				; match for . and proceeds for next token
      (begin				
	(match 'CHAR ".")       
	(let ([t1 (term)])    				; store term function's return value in t1
	  (regexp-rest (string-append "(" t t1 ")"))))  ; return whole exp's value
      t))

;;Flesh out skeleton functions below.

; term -> factor term-rest

(define (term)						; function call			
  (let ([f (factor)])					; store factor function's return value
    (term-rest f)))					; call term-rest passing f as parameter

; term-rest -> + factor term-rest | empty

(define (term-rest f)					; function call
  (if (check? 'CHAR "+")      				; match for + and proceeds for next token
      (begin
	(match 'CHAR "+")
	(let ([f1 (factor)])				; store factor function's return value in f1
	 (term-rest (string-append "(" f "|" f1 ")")))) ; return whole exp's value
	f))		


; factor -> (ugly-regexp) | * factor | chars(t)

(define (factor)					; function call
 (cond							; conditional check
  ((check? 'CHAR "(") 					; check for (
    (begin
        (match 'CHAR "(")
	(let ([expvar (ugly-regexp)])			; call ugly-regexp function	
	   (match 'CHAR ")")				; match for correct end )
	   (string-append "(" expvar ")"))))			; return whole exp's value
  ((check? 'CHAR "*")					; check for *
    (begin 
	(match 'CHAR "*")
	(let ([f (factor)])				; function call factor
	(string-append f "*"))))			; return whole exp's value
  (else 						
     (begin
	  (match 'CHARS "chars")			; match for chars
	  (match 'CHAR "(")				
	  (let ((tokenvalue (token-lexeme *lookahead*))); store token's value
	   (match 'CHAR (token-lexeme *lookahead*))	; match token 
	   (let ([quotevalue (quote-char tokenvalue)])  ; function call quote and store return value
	   (let ([finaltoken (chars quotevalue)])	; function call chars
	   (match 'CHAR ")")				; match correct end )
	   (string-append "[" finaltoken "]"))))))))	; return whole exp's value

; t -> ,t | t							 ; t stands for terminals

(define (chars quotevalue)					 ; function call
  (if (check? 'CHAR ",")					 ; check for ,
	(begin					
		(match 'CHAR ",")			
		(let ((tokenvalue (token-lexeme *lookahead*)))	 ; store token's value
		(match 'CHAR (token-lexeme *lookahead*)) 	 ; match token's value
		(let ([quotefinal (quote-char tokenvalue)])	 ; function call quote	
		(chars (string-append quotevalue quotefinal))))) ; return whole exp's value
     		quotevalue))
	
; functions end


;;;********************** Parser Utility Functions *********************

;;invoke as either (match KIND) or (match KIND LEXEME). If 
;;lookahead.kind == KIND and lookahead.lexeme == LEXEME (when LEXEME
;;is specified), then advances lookahead; else throws an exception.
(define (match . args)
  (if (apply check? args)
      (advance)
      (raise-user-error (make-error-message args))))

;;invoke as either (check? KIND) or (check? KIND LEXEME).  Returns true
;;iff lookahead.kind == KIND and lookahead.lexeme == LEXEME (when LEXEME
;;is specified).
(define (check? kind . rest)
  (and (equal? kind (token-kind *lookahead*))
       (or (equal? (length rest) 0)
	   (equal? (car rest) (token-lexeme *lookahead*)))))

;;quote first char of string if not word-char or digit.
(define (quote-char str)
  (if (regexp-match #px"^[\\w\\d]" str)
       str
      (string-append "\\" str)))

(define (advance) (set! *lookahead* (*next-token*)))

;;;************************* Error Handling ****************************

;;create error message for mismatch between *lookahead* and args
;;which is either (KIND) or (KIND LEXEME)
(define (make-error-message args)
  (let* ([kind (car args)]
	 [expected (if (equal? (length args) 2) (cadr args) kind)])
    (format "~a: syntax error at '~a', expecting '~s'"
	    (position->string (token-position *lookahead*))
	    (token-lexeme *lookahead*)
	    expected)))

(define (recover-from-error)
  (unless (or (check? '<NL>) (check? '<EOF>))
	  (advance)
	  (recover-from-error)))

;;;*************************** Main Program ****************************

(define (run-parse argv)
  (if (not (equal? (vector-length argv) 1))
      (usage)
      (begin
	(set! *next-token* (scanner (vector-ref argv 0)))
	(advance)    ;prime lookahead
	(parse))))
       
(run-parse (current-command-line-arguments))
				
				

