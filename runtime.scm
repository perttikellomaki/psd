;;;;
;;;; $Id: runtime.scm,v 1.6 1993/10/04 13:41:22 pk Exp $
;;;;
;;;; psd -- a portable Scheme debugger, version 1.1
;;;; Copyright (C) 1992 Pertti Kellomaki, pk@cs.tut.fi

;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 1, or (at your option)
;;;; any later version.

;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;;; GNU General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;; See file COPYING in the psd distribution.

;;;; 
;;;; Written by Pertti Kellomaki, pk@cs.tut.fi
;;;;
;;;; This file contains part of the runtime support for psd. The parts
;;;; that have to know about the primitive procedures of the
;;;; implementation are in the file "primitives.scm".

;;;;
;;;; $Log: runtime.scm,v $
;;;; Revision 1.6  1993/10/04  13:41:22  pk
;;;; The value returned by an expression is shown with qp.
;;;;
;;;; Revision 1.5  1993/09/30  07:46:31  pk
;;;; Moved qp to qp.scm. If we are using slib, qp comes from there instead
;;;; of qp.scm.
;;;;
;;;; Revision 1.4  1993/09/27  10:57:03  pk
;;;; Added variable *psd-break-entry-procedures*, and resetting of it in
;;;; psd-reset.
;;;;
;;;; Added procedure psd-break.
;;;;
;;;; Added handling of quote to the mini evaluator.
;;;;
;;;; psd-debug breaks execution if called with proc-entry-info with a name
;;;; in *psd-break-entry-procedures*.
;;;;
;;;; The c command now clears break on entry.
;;;;
;;;; Added remove.
;;;;
;;;; On breakpoint, either the expression to be evaluated is displayed, or
;;;; alternatively the call is displayed.
;;;;
;;;; psd-val now tells which symbol it could not find.
;;;;
;;;; Revision 1.3  1993/09/24  08:02:23  pk
;;;; Changed version number from 1.0 to 1.1.
;;;; Added loading of version.scm and announcing of version.
;;;; Added the command c on online help.
;;;;
;;;; Revision 1.2  1993/09/06  15:13:22  pk
;;;; Added the procedure psd-clear-breakpoint, and a new debugger command
;;;; `c' that clears the breakpoint at current line.
;;;;
;;;; Revision 1.1  1992/12/07  12:49:55  pk
;;;; The s command does not show evaluation of atoms any more.
;;;;
;;;;
;;;; Moved to RCS from sccs Nov 12th 1992. The SCCS log:
;;;; SCCS/s.runtime.scm:
;;;; 
;;;; D 1.15	92/07/09 14:53:54 pk	15 14	00018/00012/00424
;;;; Changed the way to avoid multiple breaks while executing a line with a
;;;; breakpoint. 
;;;; 
;;;; D 1.14	92/07/09 11:10:52 pk	14 13	00001/00001/00435
;;;; Changed the version number from 0.99 to 1.0
;;;; Hope this was not too early!
;;;; 
;;;; D 1.13	92/07/08 09:42:57 pk	13 12	00003/00001/00433
;;;; Fixed a bug that caused the 
;;;; 
;;;; D 1.12	92/07/07 11:37:57 pk	12 11	00004/00735/00430
;;;; Moved all the definitions that have to do with primitive procedures to
;;;; file "primitives.scm".
;;;; 
;;;; D 1.11	92/07/06 17:03:29 pk	11 10	00953/00915/00212
;;;; Made most of the procedures in psd closures that contain the original
;;;; values of the primitive procedures. This way, one can redefine cons,
;;;; car etc. without affecting the debugger.
;;;; 
;;;; D 1.10	92/07/06 15:08:05 pk	10 9	00140/00305/00987
;;;; Packaged qp within psd-debug.
;;;; Removed passing around column information, as it is not used anywhere.
;;;; Standard indentifiers are not settable from the command loop anymbore.
;;;; 
;;;; D 1.9	92/07/01 15:58:22 pk	9 8	00000/00001/01292
;;;; Oops. Removed some debugging code.
;;;; 
;;;; D 1.8	92/07/01 12:53:42 pk	8 7	00345/00009/00948
;;;; R4RS essential procedures now visible at debugger command loop.
;;;; Checking of c...r works.
;;;; 
;;;; D 1.7	92/07/01 12:23:06 pk	7 6	00045/00021/00912
;;;; Can now resume execution after a run time type error.
;;;; Can also return any value as the result of an expression.
;;;; 
;;;; D 1.6	92/07/01 12:02:25 pk	6 5	00528/00001/00405
;;;; Implemented run time checking of argument count and argument types.
;;;; Basically works.
;;;; 
;;;; D 1.5	92/06/26 16:52:12 pk	5 4	00193/00118/00213
;;;; Stepping by line works. Added psd-reset to reset the runtime state.
;;;; About to release.
;;;; 
;;;; D 1.4	92/06/25 15:30:43 pk	4 3	00185/00012/00146
;;;; Added qp support from slib - expressions are now always written on one
;;;; line, with ... indicating the supressed parts.
;;;; 
;;;; D 1.3	92/06/24 11:58:04 pk	3 2	00003/00002/00155
;;;; Added Aubrey Jaffer's patches for control-z.
;;;; 
;;;; D 1.2	92/06/23 12:30:57 pk	2 1	00020/00010/00137
;;;; No major changes.
;;;; 
;;;; D 1.1	92/05/27 10:23:06 pk	1 0	00147/00000/00000
;;;; date and time created 92/05/27 10:23:06 by pk
;;;; 

;;;
;;; List of breakpoints (file, line number)
;;;

(define *psd-breakpoints* '())

;;;
;;; List of procedure names with break entry enabled.
;;;

(define *psd-break-entry-procedures* '())

;;; Some state variables for the runtime. These have to be settable
;;; from the outside, so they are now here. A better solution would be
;;; to make the debugger main loop a closure that would keep track of
;;; these, and change them when requested.

(define *psd-break?* #f)            ; used for stepping thru evaluation
(define *psd-coming-from-line* #f)  ; used for stepping line by line
(define *psd-stepping-by-line* #f)
(define *psd-current-line* #f)      ; used for triggering breakpoints only
(define *psd-hits-this-line* 0)     ; once per line

;;;
;;; Reset the runtime state.
;;;

(define (psd-reset)
  (set! *psd-breakpoints* '())
  (set! *psd-break-entry-procedures* '())
  (set! *psd-break?* #f)
  (set! *psd-coming-from-line* #f)
  (set! *psd-stepping-by-line* #f)
  (set! *psd-current-line* #f)
  (set! *psd-hits-this-line* 0)
  'ok)

;;;
;;; Set a breakpoint.
;;;

(define psd-set-breakpoint 
  (let ((list list) (member member) (cons cons)
		    (string-append string-append)
		    (number->string number->string))

    (lambda (file line)
      (let ((this (list file line)))
	(if (member this *psd-breakpoints*)
	    #f
	    (set! *psd-breakpoints*
		  (cons this
			*psd-breakpoints*))))
      (string-append "Breakpoint at "
		     file
		     ":"
		     (number->string line)))))

;;;
;;; Clear a breakpoint
;;;

(define psd-clear-breakpoint
  (let ((null? null?) (equal? equal?) (car car) (cdr cdr)
		      (cons cons) (string-append string-append)
		      (number->string number->string))

    (lambda (file line)

      (define (remove breakpoint breakpoints)
	(if (null? breakpoints)
	    '()
	    (if (equal? breakpoint (car breakpoints))
		(remove breakpoint (cdr breakpoints))
		(cons breakpoint
		      (remove breakpoint (cdr breakpoints))))))
      
      (set! *psd-breakpoints* (remove (list file line) *psd-breakpoints*))
      (string-append "Removed breakpoint(s) at "
		     file
		     ":"
		     (number->string line)))))

;;;
;;; Set break on entry
;;;

(define psd-break
  (let ((cons cons) (string-append string-append)
		    (symbol->string symbol->string))
    (lambda (name)
      (set! *psd-break-entry-procedures*
	    (cons name *psd-break-entry-procedures*))
      (string-append "Break on entry to "
		     (symbol->string name)))))


;;;
;;; Back up qp to psd-qp.
;;;

(define psd-qp qp)

;;;
;;; The debugger command interpreter.
;;;

(define psd-debug 

  ;; just to make sure...
  (let ((+ +) (- -) (< <) (= =) (apply apply) (boolean? boolean?)
	      (caddr caddr) (cadr cadr) (car car) (cdr cdr) (char? char?)
	      (display display) (eq? eq?) (equal? equal?) (for-each for-each)
	      (force-output force-output) (input-port? input-port?) (list list)
	      (map map) (member member) (min min) (newline newline) (not not)
	      (null? null?) (number->string number->string) (number? number?)
	      (output-port? output-port?) (pair? pair?) (procedure? procedure?)
	      (quotient quotient) (read read) (reverse reverse)
	      (string-length string-length) (string? string?) (substring substring)
	      (symbol->string symbol->string) (symbol? symbol?)
	      (vector-length vector-length) (vector-ref vector-ref)
	      (vector? vector?) (write write))

    (lambda (val set context place
		 file-index start-line
		 end-line continuation proc-entry-info)


      ;; Convert the file index to a string
      (define file-name (psd-index->path file-index))

      ;; Prompt the user for commands
      (define (prompt)
	(display "psd> ")
	(force-output)
	(read))

      ;; Evaluator for simple procedure calls and set!
      (define (eval form)
	(cond ((and (pair? form)
		    (eq? 'set! (car form)))
	       (let ((value (eval (caddr form))))
		 (set (cadr form) value)
		 value))
	      ((and (pair? form)
		    (eq? 'quote (car form)))
	       (cadr form))
	      ((pair? form)
	       (apply (eval (car form))
		      (map eval (cdr form))))
	      ((symbol? form)
	       (val form))
	      (else
	       form)))

      ;; Show the context as file name and a list of procedure names
      (define (show-context)
	(if (null? (context))
	    (display "Top level")
	    (begin
	      (display file-name)
	      (display ":")
	      (display (reverse (context)))))
	(newline))

      ;; Show the position in a format that Emacs can parse
      (define (show-position file line)
	(display psd:control-z)
	(display psd:control-z)
	(display file)
	(display ":")
	(display line)
	(newline))

      ;; Check if there is a breakpoint for this line
      (define (break-here? file line)
	(cond

	 ;; break if there is a breakpoint for this line, and we
	 ;; have just come from somewhere else
	 ((and (member (list file line) *psd-breakpoints*)
	       (= *psd-hits-this-line* 0)))

	 ;; break also if we just entered a procedure that is listed
	 ;; in *psd-break-entry-procedures*
	 ((and proc-entry-info
	       (memq (car proc-entry-info)
		     *psd-break-entry-procedures*)))
	  
	 (else #f)))

      ;; The top level loop. The top level loop returns either false, in
      ;; which case the program is continued, or a list whose contents
      ;; should be returned as the value of the current expression.
      (define (psd-top-level file-name line entering?)
	(show-position file-name line)
	(let loop ((command (prompt)))
	  (case command
	    ((val)
	     (write (val (read)))
	     (newline)
	     (loop (prompt)))
	    ((set!)
	     (let* ((sym (read))
		    (val (read)))
	       (set sym val))
	     (loop (prompt)))
	    ((w)
	     (show-context)
	     (loop (prompt)))
	    ((s)
	     (set! *psd-stepping-by-line* #f)
	     (set! *psd-break?* #t)
	     #f)
	    ((c)
	     (display (psd-clear-breakpoint file-name line))
	     (newline)
	     (if proc-entry-info
		 (begin
		   (set! *psd-break-entry-procedures*
			 (remove (car proc-entry-info)
				 *psd-break-entry-procedures*))
		   (display "Removed break on ")
		   (display (car proc-entry-info))
		   (newline))
		 #f)
	     (loop (prompt)))
	    ((g)
	     (set! *psd-stepping-by-line* #f)
	     (set! *psd-break?* #f)
	     #f)
	    ((n)
	     (set! *psd-stepping-by-line* #t)
	     (set! *psd-break?* #f)
	     (set! *psd-coming-from-line* (list file-name line))
	     #f)
	    ((r)
	     (list (eval (read))))
	    (else
	     (if (pair? command)
		 (begin
		   (write (eval command))
		   (newline)
		   (loop (prompt)))
		 (begin
		   (display "Commands are:")(newline)
		   (display "val sym          give the value of sym")(newline)
		   (display "set! sym val     set the value of sym to val")(newline)
		   (display "g                run until the next break point")(newline)
		   (display "c                clear all breakpoints on current line")(newline)
		   (display "w                give the current context (file name and surrounding procedures)")(newline)
		   (display "s                step one step in the evaluation process")(newline)
		   (display "n                run until evaluation reaches another line")(newline)
		   (display "r expr           return expr as the value of current expression")(newline)
		   (display "                 expr can be a procedure call")(newline)
		   (newline)
		   (display "A list is taken to be a procedure call to be evaluated.")(newline)
		   (newline)
		   (loop (prompt))))))))

      ;; remove a symbol from a list. shouldn't this be essential?
      (define (remove sym lst)
	(cond ((null? lst) '())
	      ((eq? sym (car lst))
	       (remove sym (cdr lst)))
	      (else
	       (cons (car lst)
		     (remove sym (cdr lst))))))
	     

      ;; Body of psd-debug. First update the line information.
      (let ((position (list file-name start-line)))
	(if (equal? position
		    *psd-current-line*)
	    (set! *psd-hits-this-line*
		  (+ *psd-hits-this-line* 1))
	    (begin
	      (set! *psd-current-line* position)
	      (set! *psd-hits-this-line* 0))))
      
      (if (or

	   ;; if single stepping, do not show evaluation of literals
	   ;; and variables
	   (and *psd-break?* (pair? place))

	   ;; breakpoint?
	   (break-here? file-name start-line)

	   ;; stepping by line and on a new line
	   (and *psd-stepping-by-line*
		(not (equal? (list file-name start-line)
			     *psd-coming-from-line*))))

	  ;; Breakpoint or stepping
	  (begin
	    (if proc-entry-info
		
		;; if just entered a procedure, and break entry is
		;; enabled for it, show what the call looks like
		(psd-qp proc-entry-info)
		
		(psd-qp place))
	    (newline)
	    (let ((top-level-val
		   (psd-top-level file-name start-line #t)))
	      (if top-level-val

		  ;; the user wanted to return this value
		  (car top-level-val)

		  (let ((return-value (continuation)))
		    (if (or
			 ;; we are single stepping, do not show
			 ;; evaluation of literals and variables. 
			 (and *psd-break?* (pair? place))

			 ;; breakpoint?
			 (break-here? file-name end-line)

			 ;; stepping by line and we are on a new line
			 (and *psd-stepping-by-line*
				 (not (equal? (list file-name end-line)
					      *psd-coming-from-line*))))
			(begin
			  (if proc-entry-info
		
			      ;; if coming from a breaked procedure,
			      ;; show what the call looked like
			      (psd-qp proc-entry-info)
		
			      (psd-qp place))
			  (display " ==> ")
			  (qp return-value)
			  (newline)
			  (let ((top-level-val
				 (psd-top-level file-name end-line #f)))
			    (if top-level-val
				(car top-level-val)
				return-value)))

			return-value)))))

	  ;; Check if we were called from psd-apply with a #f continuation.
	  (if continuation
	      (continuation)
	      (let loop ((val
			  (psd-top-level file-name start-line #t)))
		(if val
		    (car val)
		    (begin
		      (display "This expression can not be evaluated normally.")(newline)
		      (display "You have to specify a return value, if you want to continue execution.")
		      (newline)
		      (loop 
		       (psd-top-level file-name start-line #f))))))))))


;;;
;;; Top level definitions of psd-val, psd-set! and psd-context. Each
;;; time a file is instrumented, two procedures that map the names of
;;; all top level definitions in the file are written into the file
;;; that contains the instrumented procedures. When the file is
;;; loaded, these procedures are added to the lists
;;; psd-global-symbol-accessors and psd-global-symbol-setters. If the
;;; user wants to access a global variable, the procedures in the
;;; appropriate list are called one by one. If none of them has access
;;; to the variable, the user is notified. The accessor procedures
;;; return either a list containing the result or #f (I would much
;;; rather return two values here... sigh!). The setter
;;; procedures return #t or #f.
;;;

(define psd-val
  (let ((null? null?) (display display) (newline newline)
		      (car car) (cdr cdr))
    (lambda (sym)
      (let loop ((procs psd-global-symbol-accessors))
	(if (null? procs)
	    (begin (display sym)
		   (display " is not visible to psd.")
		   (newline)
		   #f)
	    (let ((result ((car procs) sym)))
	      (if result
		  (car result)
		  (loop (cdr procs)))))))))

(define psd-set!
  (let ((null? null?) (display display) (newline newline)
		      (car car) (cdr cdr))
    (lambda (sym val)
      (let loop ((procs psd-global-symbol-setters))
	(if (null? procs)
	    (begin (display "Symbol is not visible to psd.")
		   (newline)
		   #f)
	    (if ((car procs) sym val)
		#f
		(loop (cdr procs))))))))

