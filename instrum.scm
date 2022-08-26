;;;;
;;;; $Id: instrum.scm,v 1.7 1993/10/04 13:43:33 pk Exp $
;;;;
;;;; psd -- a portable Scheme debugger, version 1.1
;;;; Copyright (C) 1992 Pertti Kellomaki, pk@cs.tut.fi

;;;;  Written by Pertti Kellomaki, pk@cs.tut.fi

;;;; This file contains the actual instrumentation code.  For each
;;;; syntactic form X to be instrumented, there is a corresponding
;;;; procedure instrument-X. In addition, there are a few helpful
;;;; procedures for instrumenting expression sequences etc. The
;;;; procedures psd-car, psd-cdr etc. that are used here work just like
;;;; normal car, cdr etc., but they operate on pexps, which are sexps
;;;; with position information.  
;;;;
;;;; $Log: instrum.scm,v $
;;;; Revision 1.7  1993/10/04  13:43:33  pk
;;;; Fixed quasiquote instrumentation.
;;;; Added #f as the proc-entry-info argument to the list constructed by
;;;; psd-apply.
;;;; Fixed write-expr to handle nonproper lists properly.
;;;;
;;;; Revision 1.6  1993/10/04  09:47:07  pk
;;;; If the procedure has a rest argument, it is converted to a list before
;;;; passing it to psd-debug. Psd can again handle procedures with rest
;;;; arguments.
;;;;
;;;; Fixed the debugger arguments to psd-apply to include also
;;;; proc-entry-info.
;;;;
;;;; Revision 1.5  1993/09/27  10:50:07  pk
;;;; instrument-expr now handles the first expression of a procedure body
;;;; specially. Evaluating the first subexpression of a procedure body
;;;; calls psd-debug with the name of the procedure and the values of the
;;;; arguments. This way we can set break on entry to a named procedure,
;;;; and see which arguments it was called with.
;;;;
;;;; Revision 1.4  1993/09/24  07:57:46  pk
;;;; Changed version number to 1.1.
;;;;
;;;; Revision 1.3  1993/09/06  15:11:58  pk
;;;; Renamed instrument-file to psd-instrument-file.
;;;; Added a call to psd-reset-read to psd-instrument-file. Now the debug
;;;; file command works again!
;;;;
;;;; Revision 1.2  1992/12/07  12:24:47  pk
;;;; Used to think that a let with no bindings is a named let. Fixed that.
;;;;
;;;; Revision 1.1  1992/12/07  12:20:31  pk
;;;; Avoid excessively long lines by handling writing of lists in
;;;; instrument-file.
;;;;
;;;;
;;;; Moved to RCS from sccs Nov 12th 1992. The SCCS log:
;;;; SCCS/s.instrum.scm:
;;;; 
;;;; D 1.19	92/10/02 09:19:41 pk	19 18	00002/00002/00616
;;;; Removed error from the procedures that are closed over. In MIT Scheme
;;;; error is a special form, and this caused problems.
;;;; 
;;;; D 1.18	92/10/02 09:09:20 pk	18 17	00002/00002/00616
;;;; Replaced error with psd-error.
;;;; 
;;;; D 1.17	92/07/10 11:32:39 pk	17 16	00001/00001/00617
;;;; Fixed one pmap to be psd-map.
;;;; 
;;;; D 1.16	92/07/09 14:15:17 pk	16 15	00000/00001/00618
;;;; Oops, forgot a debugging display expression in the file.
;;;; 
;;;; D 1.15	92/07/09 14:13:18 pk	15 14	00002/00001/00617
;;;; Corrected the psd-val and psd-set definitions for a lambda.
;;;; 
;;;; D 1.14	92/07/09 13:16:31 pk	14 13	00001/00000/00617
;;;; Added support for vector constants.
;;;; 
;;;; D 1.13	92/07/09 11:09:54 pk	13 12	00001/00001/00616
;;;; Changed the version number from 0.99 to 1.0
;;;; Hope this was not too early!
;;;; 
;;;; D 1.12	92/07/09 11:00:23 pk	12 11	00208/00208/00409
;;;; Changed the names pcar, pcdr etc. to psd-car, psd-cdr etc.
;;;; It does not look nice, but this way the only visible names start with
;;;; psd- or *psd-. This is important.
;;;; 
;;;; D 1.11	92/07/08 14:56:49 pk	11 10	00004/00002/00613
;;;; Replaced (list ...) with ((lambda x x) ...) in the instrumented code.
;;;; 
;;;; D 1.10	92/07/07 12:25:25 pk	10 9	00000/00003/00615
;;;; Removed column arguments from call to psd-apply.
;;;; 
;;;; D 1.9	92/07/07 12:06:55 pk	9 8	00001/00007/00617
;;;; Corrected the list of closed-in variables for instrument-file.
;;;; 
;;;; D 1.8	92/07/06 17:03:00 pk	8 7	00373/00356/00251
;;;; Made most of the procedures in psd closures that contain the original
;;;; values of the primitive procedures. This way, one can redefine cons,
;;;; car etc. without affecting the debugger.
;;;; 
;;;; D 1.7	92/07/06 15:01:41 pk	7 6	00356/00355/00251
;;;; Packaged all procedures within instrument-file.
;;;; Does not put column information in the instrumented code, because it
;;;; is not used by the runtime at the moment.
;;;; 
;;;; D 1.6	92/07/01 12:02:17 pk	6 5	00021/00003/00585
;;;; Implemented run time checking of argument count and argument types.
;;;; Basically works.
;;;; 
;;;; D 1.5	92/06/30 11:47:42 pk	5 4	00078/00069/00510
;;;; Fixed bug in instrumenting internal defines.
;;;; 
;;;; D 1.4	92/06/29 16:48:04 pk	4 3	00084/00054/00495
;;;; Now instruments do by wrapping lets all around the place. Sort of
;;;; clumsy, but it works.
;;;; 
;;;; D 1.3	92/06/26 16:50:38 pk	3 2	00078/00024/00471
;;;; Now instruments all syntactic forms except unquotations, => and delay.
;;;; About to release.
;;;; 
;;;; D 1.2	92/06/23 12:29:36 pk	2 1	00167/00081/00328
;;;; Now handles most special forms and internal defines.
;;;; 
;;;; D 1.1	92/05/27 10:23:03 pk	1 0	00409/00000/00000
;;;; date and time created 92/05/27 10:23:03 by pk

;;;
;;; Instrument source-file, writing the instrumented version to
;;; instrumented-file.
;;; 

(define psd-instrument-file
  
  (let ((caadr caadr) (caddr caddr) (cadr cadr) (car car) (cdr cdr)
		      (close-output-port close-output-port) (cons cons)
		      (eof-object? eof-object?) (eq? eq?)
		      (list list) (map map) (newline newline) (not not)
		      (null? null?) (open-input-file open-input-file)
		      (open-output-file open-output-file) (pair? pair?)
		      (reverse reverse) (write write))
    
    
    (lambda (source-file instrumented-file)
      
;;;
;;; Instrument an expression. If it is the body of a named procedure,
;;; insert code to show how it was called.
;;;
      
      (define (instrument-expr expr proc-entry-info)
	
	(define (wrap instrumented-expr)
	  
	  (define file-name car)	; for accessing the position info
	  (define line-number cadr)
	  (define column caddr)

	  ;; make sure the last cdr is the empty list
	  (define (normalize lst)
	    (cond ((null? lst) lst)
		  ((pair? lst) (cons (car lst)
				     (normalize (cdr lst))))
		  (else
		   (list lst))))

	  `(psd-debug psd-val psd-set! psd-context
		      ',(pexp->sexp expr)
		      ,(file-name (psd-expr-start expr))
		      ,(line-number (psd-expr-start expr))
		      ,(line-number (psd-expr-end expr))
		      (lambda () ,instrumented-expr)
		      ,(if proc-entry-info
			   `((lambda x x)
			     ',(car proc-entry-info)
			     ,@(normalize (cdr proc-entry-info)))
			   #f)))
	
	(cond
	 
	 ;; expressions of the form (symbol ...) that are
	 ;; potential special forms
	 ((and (psd-pair? expr)
	       (psd-symbol? (psd-car expr)))
	  (case (psd-expr-contents (psd-car expr))
	    ((and) (wrap (instrument-and expr)))
	    ((begin) (wrap (instrument-begin expr)))
	    ((case) (wrap (instrument-case expr)))
	    ((cond) (wrap (instrument-cond expr)))
	    ((define) (instrument-define expr))
	    ((do) (instrument-do expr))
	    ((if) (wrap (instrument-if expr)))
	    ((lambda) (wrap (instrument-lambda expr)))
	    ((let) (wrap (instrument-let expr)))
	    ((let*) (wrap (instrument-let* expr)))
	    ((letrec) (wrap (instrument-letrec expr)))
	    ((or) (wrap (instrument-or expr)))
	    ((quasiquote) (wrap (instrument-quasiquote expr)))
	    ((quote) (instrument-quote expr))
	    ((set!) (wrap (instrument-set! expr)))
	    
	    ;; anything we don't recognize must be a procedure call
	    (else
	     (wrap (instrument-call expr)))))
	 
	 ;; list that starts with a list must be a procedure call
	 ;; somewhere deep down 
	 ((psd-pair? expr)
	  (wrap (instrument-call expr)))
	 
	 ;; ordinary atoms
	 ((or (psd-symbol? expr)
	      (psd-number? expr)
	      (psd-string? expr)
	      (psd-vector? expr)
	      (psd-char? expr)
	      (psd-boolean? expr)
	      (psd-null? expr))
	  (wrap (pexp->sexp expr)))
	 
	 (else
	  (error "Can not handle " expr))))
      
      
;;;
;;; A wrapper that has visibility to all the given symbols is needed
;;; in a few places.
;;; 
      
      (define (access-wrapper exprs variables)
	`(let ,(build-val-set-definitions variables)
	   ,@exprs))
      
;;;
;;; Build definitions for psd-val and psd-set!
;;; Beware that variables can be a list of variables, a single
;;; variable or a list of the form (a b . c)
;;;
;;; The definitions are of the form
;;; 
;;;  ((psd-val (lambda (...) ...))
;;;   (psd-set! (lambda (...) ...)))
;;;   
;;; suitable for inclusion in a let form.
;;;
      
      (define (build-set-definition variables)
	(let loop ((variables variables)
		   (set-body '()))
	  (cond ((null? variables)
		 `(lambda (psd-temp psd-temp2)
		    (case psd-temp
		      ,@set-body
		      (else (psd-set! psd-temp psd-temp2)))))
		(else
		 (loop (cdr variables)
		       (cons `((,(car variables)) (set! ,(car variables) psd-temp2))
			     set-body))))))
      
      (define (build-val-definition variables)
	(let loop ((variables variables)
		   (val-body '()))
	  (cond ((null? variables)
		 `(lambda (psd-temp)
		    (case psd-temp
		      ,@val-body
		      (else (psd-val psd-temp)))))
		(else
		 (loop (cdr variables)
		       (cons `((,(car variables)) ,(car variables))
			     val-body))))))
      
      (define (build-val-set-definitions variables)
	
	;; build a proper list out of the variable list, that can be a
	;; single symbol, list, or dotted list
	(define (make-proper-list maybe-dotted-list)
	  (cond ((null? maybe-dotted-list) '())
		((pair? maybe-dotted-list)
		 (cons (car maybe-dotted-list)
		       (make-proper-list (cdr maybe-dotted-list))))
		(else
		 (list maybe-dotted-list))))
	
	(let ((variables (make-proper-list variables)))
	  `((psd-val ,(build-val-definition variables))
	    (psd-set! ,(build-set-definition variables)))))
      
      
;;;
;;; A set! is instrumented by instrumenting the value.
;;; 
      
      (define (instrument-set! expr)
	(let ((var (pexp->sexp (psd-cadr expr)))
	      (val (psd-caddr expr)))
	  `(set! ,var ,(instrument-expr val #f))))
      
;;;
;;; Quote and quasiquote. We don't try to instrument anything
;;; that is inside a quasiquote.
;;;
      
      (define (instrument-quasiquote expr) 
	(list 'quasiquote (pexp->sexp (psd-cadr expr))))
      
      (define (instrument-quote expr)
	`(quote ,(pexp->sexp (psd-cadr expr))))
      
;;;
;;; A body (expression sequece) is instrumented by instrumenting each
;;; of the expressions. If there are internal defines, they are turned
;;; into an equivalent letrec form and access procedures for them are
;;; also generated. If procedure-info is not null, this body is a
;;; procedure body, and procedure-info contains the name and arguments
;;; of the procedure.
;;;
      
      (define (instrument-body body . procedure-info)
	
	;; Return the leading definitions as a list of pexps
	(define (leading-definitions body)
	  (let loop ((body body)
		     (definitions '()))
	    (cond ((psd-null? body)
		   (reverse definitions))
		  ((and (psd-pair? (psd-car body))
			(eq? 'define
			     (pexp->sexp (psd-caar body))))
		   (loop (psd-cdr body)
			 (cons (psd-car body)
			       definitions)))
		  (else
		   (reverse definitions)))))
	
	;; Return the rest of the body as a pexp
	(define (trailing-exprs body)
	  (let loop ((body body))
	    (cond ((psd-null? body) body)
		  ((and (psd-pair? (psd-car body))
			(eq? 'define
			     (pexp->sexp (psd-caar body))))
		   (loop (psd-cdr body)))
		  (else body))))
	
	;; Given a define form, return a corresponding binding for a letrec
	(define (build-letrec-binding definition variables)
	  `(,(definition-name definition)
	    ,(access-wrapper (list (build-definition-body definition)) variables)))
	
	
	;; If there are no internal definitions, do not wrap a redundant letrec
	;; around the body
	(let ((definitions (leading-definitions body)))
	  (if (null? definitions)
	      
	      (build-body body procedure-info)
	      
	      ;; there were definitions, so we must wrap a letrec around the
	      ;; expressions that make up the body
	      (let ((variables (map definition-name definitions)))
		`((letrec ,(map (lambda (binding)
				  (build-letrec-binding binding variables))
				definitions)
		    ,(access-wrapper
		      (build-body (trailing-exprs body) procedure-info)
		      variables)))))))

;;;
;;; Build a body, with special treatment for the first expression if
;;; it is a procedure body.

      (define (build-body expr-sequence procedure-info)
	(if (null? procedure-info)
	    (psd-map (lambda (expr) (instrument-expr expr #f))
		     expr-sequence)
	    (cons (instrument-expr (psd-car expr-sequence )
				   (car procedure-info))
		  (psd-map (lambda (expr) (instrument-expr expr #f))
			   (psd-cdr expr-sequence)))))
		   
	
      
;;;
;;; Instrument (and ...)
;;;
      
      (define (instrument-and form)
	(cons 'and (psd-map (lambda (expr) (instrument-expr expr #f))
			    (psd-cdr form))))
      
;;;
;;; Instrument (or ...)
;;;
      
      (define (instrument-or form)
	(cons 'or (psd-map (lambda (expr) (instrument-expr expr #f))
			   (psd-cdr form))))
      
;;;
;;; Instrument (do ...)
;;; This is rather messy, because of the scoping rules of the do form.
;;; There is no convinient place to put the access procedures so that
;;; all variables would be visible at all times.
;;;
;;; The problem is that all the variables are visible at the update
;;; forms but not at the init forms. For this reason we have to wrap a
;;; let around every update form in order to get to the right values.
;;; The same applies to the test and result forms.
;;;
      
      (define (instrument-do form)
	
	;; Instrument a do variable binding
	(define (instrument-do-binding binding variables)
	  (let ((variable (pexp->sexp (psd-car binding)))
		(init (psd-cadr binding))
		(step
		 (if (psd-null? (psd-cddr binding))
		     (psd-car binding)
		     (psd-caddr binding))))
	    `(,variable ,(instrument-expr init #f)
			,(access-wrapper (list (instrument-expr step #f))
					 variables))))
	
	
	(let ((bindings (psd-cadr form))
	      (variables (let-variables (psd-cadr form)))
	      (test-result (psd-caddr form))
	      (body (psd-cdddr form)))
	  
	  `(do ,(psd-map (lambda (binding)
			   (instrument-do-binding binding variables))
			 bindings)
	       ,(psd-map (lambda (expr) (instrument-expr expr #f))
			 test-result)
	     ,@(instrument-body body))))
      
      
;;;
;;; Instrument (begin ...)
;;; 
      
      (define (instrument-begin form)
	(cons 'begin (instrument-body (psd-cdr form))))
      
;;;
;;; Instrument a let, let* or letrec binding list.
;;;
      
      (define (instrument-let-bindings bindings)
	(let loop ((bindings bindings)
		   (result '()))
	  (if (psd-null? bindings)
	      (reverse result)
	      (let ((var (psd-expr-contents (psd-caar bindings)))
		    (expr (psd-cadar bindings)))
		(loop (psd-cdr bindings)
		      (cons (list var
				  (instrument-expr expr #f))
			    result))))))
      
;;;
;;; Return a list of variables being bound in a binding list.
;;;
      
      (define (let-variables bindings)
	(psd-map (lambda (binding)
		   (psd-expr-contents (psd-car binding)))
		 bindings))
      
;;;
;;; Instrument a let, let* or letrec form. We have to be aware of
;;; named let. 
;;;
      
      (define (instrument-let form)
	(instrument-let-form form 'let))
      
      (define (instrument-let* form)
	(instrument-let-form form 'let*))
      
      (define (instrument-letrec form)
	(instrument-let-form form 'letrec))
      
      
      (define (instrument-let-form form keyword)
	(let ((bindings (if (or (psd-pair? (psd-cadr form))
				(psd-null? (psd-cadr form)))
			    (psd-cadr form)
			    (psd-caddr form)))
	      (name (if (or (psd-pair? (psd-cadr form))
			    (psd-null? (psd-cadr form)))
			'()
			(list (pexp->sexp (psd-cadr form)))))
	      (body (if (or (psd-pair? (psd-cadr form))
			    (psd-null? (psd-cadr form)))
			(psd-cddr form)
			(psd-cdddr form))))
	  `(,keyword ,@name ,(instrument-let-bindings bindings)
		     (let ,(build-val-set-definitions (let-variables bindings))
		       ,@(instrument-body body)))))
      
      
;;;
;;; Instrument a lambda.
;;;
      
      (define (instrument-lambda form)
	(let ((variables (psd-cadr form))
	      (body (psd-cddr form)))
	  `(lambda ,(pexp->sexp variables)
	     (let ,(build-val-set-definitions (psd-map pexp->sexp variables))
	       ,@(instrument-body body)))))
      
;;;
;;; Return the name of the variable being defined in a definition.
;;; 
      (define (definition-name definition)
	(let ((variable (psd-cadr definition)))
	  (pexp->sexp
	   (if (psd-pair? variable)
	       (psd-car variable)
	       variable))))
      
;;;
;;; Build an instrumented body that corresponds to the definition. We
;;; need to be aware of (define foo ...) and (define (foo ...) ...).
;;;
;;; For each procedure definition of the form
;;; (define (foo x) ...) we supply a procedure definition that will
;;; give the name of this and surrounding procedures.
;;;
      
      (define (build-definition-body form)
	(if (psd-pair? (psd-car (psd-cdr form)))
	    
	    ;; we have a (define (foo x) ...)
	    (let* ((heading (psd-car (psd-cdr form)))
		   (proc-name (psd-expr-contents (psd-car heading)))
		   (arguments (psd-map psd-expr-contents (psd-cdr heading)))
		   (body (psd-cdr (psd-cdr form))))
	      `(let ((psd-context
		      (lambda () (cons ',proc-name
				       (psd-context)))))
		 (lambda ,arguments
		   (let ,(build-val-set-definitions arguments)
		     ,@(instrument-body body
					(cons proc-name
					      arguments))))))
	    
	    ;; we have a (define foo ...)
	    (let ((expr (psd-caddr form)))
	      (instrument-expr expr #f))))
      
;;;
;;; Instrument a define.
;;;
      
      (define (instrument-define form)
	`(define ,(definition-name form) ,(build-definition-body form)))
      
;;;
;;; Instrument an if.
;;;
      
      (define (instrument-if form)
	(let ((condition (psd-car (psd-cdr form)))
	      (then-branch (psd-car (psd-cdr (psd-cdr form))))
	      (else-branch
	       (if (psd-null? (psd-cdr (psd-cdr (psd-cdr form))))
		   #f
		   (psd-car (psd-cdr (psd-cdr (psd-cdr form)))))))
	  (if else-branch
	      `(if ,(instrument-expr condition #f)
		   ,(instrument-expr then-branch #f)
		   ,(instrument-expr else-branch #f))
	      `(if ,(instrument-expr condition #f)
		   ,(instrument-expr then-branch #f)))))
      
;;;
;;; Instrument a cond.
;;;
      
      (define (instrument-cond expr)
	
	(define (instrument-cond-clause clause)
	  (cond
	   
	   ;; else clause
	   ((and (psd-symbol? (psd-car clause))
		 (eq? (pexp->sexp (psd-car clause))
		      'else))
	    `(else ,@(instrument-body (psd-cdr clause))))
	   
	   ;; clause with just the predicate part
	   ((psd-null? (psd-cdr clause))
	    `(,(instrument-expr (psd-car clause) #f)))
	   
	   ;; ordinary clause
	   (else
	    `(,(instrument-expr (psd-car clause) #f)
	      ,@(instrument-body (psd-cdr clause))))))
	
	`(cond ,@(psd-map instrument-cond-clause (psd-cdr expr))))
      
;;;
;;; Instrument a case.
;;;
      
      (define (instrument-case expr)
	
	(define (instrument-case-clause clause)
	  (cond
	   
	   ;; else clause
	   ((and (psd-symbol? (psd-car clause))
		 (eq? (pexp->sexp (psd-car clause))
		      'else))
	    `(else ,@(instrument-body (psd-cdr clause))))
	   
	   ;; ordinary clause
	   (else
	    `(,(pexp->sexp (psd-car clause)) ,@(instrument-body (psd-cdr clause))))))
	
	`(case ,(instrument-expr (psd-cadr expr) #f)
	   ,@(psd-map instrument-case-clause (psd-cddr expr))))
      
;;;
;;; Instrument a procedure call. In case the call would cause a run
;;; time error, all the necessary information for invoking the
;;; debugger command loop is passed to psd-apply also. The value #f in
;;; the continuation position indicates to the command loop that the
;;; program can only be continued with a user supplied return value
;;; for the call.
;;;
      
      (define (instrument-call expr)
	
	(define file-name car)		; for accessing the position info
	(define line-number cadr)
	
	;; (lambda x x) is used instead of list just in case someone
	;; wants to redefine list
	`(psd-apply ((lambda x x) ,@(psd-map (lambda (expr)
					       (instrument-expr expr
								#f))
					     expr))
		    psd-val psd-set! psd-context
		    ',(pexp->sexp expr)
		    ,(file-name (psd-expr-start expr))
		    ,(line-number (psd-expr-start expr))
		    ,(line-number (psd-expr-end expr))
		    #f
		    #f))
      
      
;;;
;;; Each instrumented file contains procedures that map the names of
;;; the top level symbols to the corresponding variables.
;;; 
      
      (define (build-global-accessors file-name)
	
	(define (build-accessor expr branches)
	  (if (or (not (pair? expr))
		  (not (eq? 'define (car expr))))
	      
	      ;; this was not a definition
	      branches
	      
	      ;; now we have to distinguis between (define foo ..) and
	      ;; (define (foo ...) ...)
	      (let ((var (if (pair? (cadr expr))
			     (caadr expr)
			     (cadr expr))))
		(cons `((,var) ((lambda x x) ,var))
		      branches))))
	
	
	(let ((port (open-input-file file-name)))
	  (let loop ((expr (pexp->sexp (psd-read port file-name)))
		     (case-branches '()))
	    (if (eof-object? expr)
		
		`(set! psd-global-symbol-accessors
		       (cons (lambda (psd-temp)
			       (case psd-temp
				 ,@case-branches
				 (else #f)))
			     psd-global-symbol-accessors))
		
		(loop (pexp->sexp (psd-read port file-name))
		      (build-accessor expr case-branches))))))
      
      
      (define (build-global-setters file-name)
	
	(define (build-setter expr branches)
	  (if (or (not (pair? expr))
		  (not (eq? 'define (car expr))))
	      
	      ;; this was not a definition
	      branches
	      
	      ;; now we have to distinguis between (define foo ..) and
	      ;; (define (foo ...) ...)
	      (let ((var (if (pair? (cadr expr))
			     (caadr expr)
			     (cadr expr))))
		(cons `((,var) (set! ,var psd-temp2))
		      branches))))
	
	
	(let ((port (open-input-file file-name)))
	  (let loop ((expr (pexp->sexp (psd-read port file-name)))
		     (case-branches '()))
	    (if (eof-object? expr)
		
		`(set! psd-global-symbol-setters
		       (cons (lambda (psd-temp psd-temp2)
			       (case psd-temp
				 ,@case-branches
				 (else #f)))
			     psd-global-symbol-setters))
		
		(loop (pexp->sexp (psd-read port file-name))
		      (build-setter expr case-branches))))))
      
      


;;;
;;; Write an expr avoiding long lines. The result is not intended for
;;; human eyes! 
;;;

      (define (write-expr expr port)
	(cond ((pair? expr)
	       (display "(" port)
	       (let loop ((expr expr))
		 (cond ((null? expr)
			(display ")" port))
		       ((pair? expr)
			(write-expr (car expr) port)
			(newline port)
			(loop (cdr expr)))
		       (else
			(display " . " port)
			(write-expr expr port)
			(display ")" port)))))
	      (else
	       (write expr port))))

      
      
;;;
;;; Body of instrument-file
;;;
      
      (let* ((infile (open-input-file source-file))
	     (outfile (open-output-file instrumented-file)))
	(psd-reset-read)
	;; added so slib PROGRAM-VICINITY will work for instrument files:
	(write `(define *load-pathname* ,source-file) outfile)
	(let loop ((expr (psd-read infile source-file)))
	  (if (eof-object? expr)
	      (begin
		(write-expr (build-global-accessors source-file)
			    outfile)
		(newline outfile)
		(write-expr (build-global-setters source-file)
			    outfile)
		(newline outfile)
		(close-output-port outfile))
	      (begin
		(write-expr (instrument-expr expr #f)
			    outfile)
		(newline outfile)
		(loop (psd-read infile source-file)))))))))
