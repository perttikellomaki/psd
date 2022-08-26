;;;;
;;;; $Id: primitives.scm,v 1.6 1994/02/02 06:41:48 pk Exp $
;;;;
;;;; psd -- a portable Scheme debugger, version 1.1
;;;; Copyright (C) 1992 Pertti Kellomaki, pk@cs.tut.fi

;;;; 
;;;; Written by Pertti Kellomaki, pk@cs.tut.fi
;;;;
;;;; This file contains the parts of the runtime support that have to
;;;; know about the primitive procedures in the interpreter. This file
;;;; only knows about the primitives listed as essential in the R4RS.
;;;; If you want to add other primitives found in a particular
;;;; implementation, add them in this file.

;;;;
;;;; $Log: primitives.scm,v $
;;;; Revision 1.6  1994/02/02  06:41:48  pk
;;;; Removed references to *psd-previous-line*. Thanks to Jonathan Rees.
;;;;
;;;; Revision 1.5  1994/02/02  06:35:24  pk
;;;; Corrected the number of arguments to write-char. Thanks to Jonathan
;;;; Rees.
;;;;
;;;; Revision 1.4  1993/09/27  10:51:10  pk
;;;; Added psd-break to the list of visible global symbols.
;;;;
;;;; Revision 1.3  1993/09/24  07:57:56  pk
;;;; Changed version number to 1.1.
;;;;
;;;; Revision 1.2  1993/09/06  15:06:55  pk
;;;; Added psd-clear-breakpoint to the list of visible procedures.
;;;;
;;;; Revision 1.1  1993/05/05  12:51:17  pk
;;;; Changed the procedure list? in psd-apply to psd-list?, because list?
;;;; conflicted with the builtin list?.
;;;;
;;;;
;;;; Moved to RCS from sccs Nov 12th 1992. The SCCS log:
;;;; SCCS/s.primitives.scm:
;;;; 
;;;; D 1.12	92/07/28 10:35:21 pk	12 11	00001/00001/00810
;;;; Corrected a typo in the signature of substring: string -> string?
;;;; 
;;;; D 1.11	92/07/17 11:26:13 pk	11 10	00023/00014/00788
;;;; Changed the predicate pair? to list? in the signatures of append,
;;;; assoc, assq, assv, length, list->string, list->vector, list-ref,
;;;; member, memq, memv and reverse.
;;;; 
;;;; D 1.10	92/07/17 11:16:12 pk	10 9	00001/00001/00801
;;;; Really fixed the previous delta.
;;;; 
;;;; D 1.9	92/07/17 11:12:28 pk	9 8	00001/00001/00801
;;;; Fixed signature of for-each.
;;;; 
;;;; D 1.8	92/07/16 13:42:01 pk	8 7	00012/00004/00790
;;;; Fixed signatures for display and write in type checking.
;;;; Added error message for number->string and string->number.
;;;; 
;;;; D 1.7	92/07/09 14:18:38 pk	7 6	00002/00002/00792
;;;; set-car! and set-cdr! do take two arguments, not one like the check list
;;;; used to tell.
;;;; 
;;;; D 1.6	92/07/09 13:16:44 pk	6 5	00001/00001/00793
;;;; Fixed checking for a fixed number of arguments.
;;;; *** CHANGED *** 92/07/09 13:27:21 pk
;;;; Added support for vector constants.
;;;; 
;;;; D 1.5	92/07/09 11:10:13 pk	5 4	00001/00001/00793
;;;; Changed the version number from 0.99 to 1.0
;;;; Hope this was not too early!
;;;; 
;;;; D 1.4	92/07/08 12:38:12 pk	4 3	00014/00005/00780
;;;; Added check for application of non procedure.
;;;; 
;;;; D 1.3	92/07/07 13:40:04 pk	3 2	00001/00011/00784
;;;; Moved explanation of implementation specific initialization to
;;;; psd.scm.
;;;; 
;;;; D 1.2	92/07/07 12:28:09 pk	2 1	00223/00223/00572
;;;; Fixed a misplaced paren.
;;;; 
;;;; D 1.1	92/07/07 11:38:21 pk	1 0	00795/00000/00000
;;;; date and time created 92/07/07 11:38:21 by pk

;;;
;;; Originally only psd globals and R4RS essentials are visible.
;;; 

(define psd-global-symbol-accessors
  (list (lambda (psd-temp)
	  (case psd-temp

	    ;; Put additional global symbols here. For example, to
	    ;; make the symbol list-tail visible, add the line
	    ;; ((list-tail) `(,list-tail))

	    ;; r4rs essentials
	    ((*) `(,*))
	    ((+) `(,+))
	    ((-) `(,-))
	    ((/) `(,/))
	    ((<) `(,<))
	    ((<=) `(,<=))
	    ((=) `(,=))
	    ((>) `(,>))
	    ((>=) `(,>=))
	    ((abs) `(,abs))
	    ((append) `(,append))
	    ((apply) `(,apply))
	    ((assoc) `(,assoc))
	    ((assq) `(,assq))
	    ((assv) `(,assv))
	    ((boolean?) `(,boolean?))
	    ((caaaar) `(,caaaar))
	    ((caaadr) `(,caaadr))
	    ((caaar) `(,caaar))
	    ((caadar) `(,caadar))
	    ((caaddr) `(,caaddr))
	    ((caadr) `(,caadr))
	    ((caar) `(,caar))
	    ((cadaar) `(,cadaar))
	    ((cadadr) `(,cadadr))
	    ((cadar) `(,cadar))
	    ((caddar) `(,caddar))
	    ((cadddr) `(,cadddr))
	    ((caddr) `(,caddr))
	    ((cadr) `(,cadr))
	    ((call-with-current-continuation) `(,call-with-current-continuation))
	    ((call-with-input-file) `(,call-with-input-file))
	    ((call-with-output-file) `(,call-with-output-file))
	    ((car) `(,car))
	    ((cdaaar) `(,cdaaar))
	    ((cdaadr) `(,cdaadr))
	    ((cdaar) `(,cdaar))
	    ((cdadar) `(,cdadar))
	    ((cdaddr) `(,cdaddr))
	    ((cdadr) `(,cdadr))
	    ((cdar) `(,cdar))
	    ((cddaar) `(,cddaar))
	    ((cddadr) `(,cddadr))
	    ((cddar) `(,cddar))
	    ((cdddar) `(,cdddar))
	    ((cddddr) `(,cddddr))
	    ((cdddr) `(,cdddr))
	    ((cddr) `(,cddr))
	    ((cdr) `(,cdr))
	    ((ceiling) `(,ceiling))
	    ((char->integer) `(,char->integer))
	    ((char-alphabetic?) `(,char-alphabetic?))
	    ((char-ci<=?) `(,char-ci<=?))
	    ((char-ci<?) `(,char-ci<?))
	    ((char-ci=?) `(,char-ci=?))
	    ((char-ci>=?) `(,char-ci>=?))
	    ((char-ci>?) `(,char-ci>?))
	    ((char-downcase) `(,char-downcase))
	    ((char-lower-case?) `(,char-lower-case?))
	    ((char-numeric?) `(,char-numeric?))
	    ((char-upcase) `(,char-upcase))
	    ((char-upper-case?) `(,char-upper-case?))
	    ((char-whitespace?) `(,char-whitespace?))
	    ((char<=?) `(,char<=?))
	    ((char<?) `(,char<?))
	    ((char=?) `(,char=?))
	    ((char>=?) `(,char>=?))
	    ((char>?) `(,char>?))
	    ((char?) `(,char?))
	    ((close-input-port) `(,close-input-port))
	    ((close-output-port) `(,close-output-port))
	    ((complex?) `(,complex?))
	    ((cons) `(,cons))
	    ((current-input-port) `(,current-input-port))
	    ((current-output-port) `(,current-output-port))
	    ((display) `(,display))
	    ((eof-object?) `(,eof-object?))
	    ((eq?) `(,eq?))
	    ((equal?) `(,equal?))
	    ((eqv?) `(,eqv?))
	    ((even?) `(,even?))
	    ((exact?) `(,exact?))
	    ((floor) `(,floor))
	    ((for-each) `(,for-each))
	    ((gcd) `(,gcd))
	    ((inexact?) `(,inexact?))
	    ((input-port?) `(,input-port?))
	    ((integer->char) `(,integer->char))
	    ((integer?) `(,integer?))
	    ((lcm) `(,lcm))
	    ((length) `(,length))
	    ((list) `(,list))
	    ((list->string) `(,list->string))
	    ((list->vector) `(,list->vector))
	    ((list-ref) `(,list-ref))
	    ((list?) `(,list?))
	    ((load) `(,load))
	    ((make-string) `(,make-string))
	    ((make-vector) `(,make-vector))
	    ((map) `(,map))
	    ((max) `(,max))
	    ((member) `(,member))
	    ((memq) `(,memq))
	    ((memv) `(,memv))
	    ((min) `(,min))
	    ((modulo) `(,modulo))
	    ((negative?) `(,negative?))
	    ((newline) `(,newline))
	    ((not) `(,not))
	    ((null?) `(,null?))
	    ((number->string) `(,number->string))
	    ((number?) `(,number?))
	    ((odd?) `(,odd?))
	    ((open-input-file) `(,open-input-file))
	    ((open-output-file) `(,open-output-file))
	    ((output-port?) `(,output-port?))
	    ((pair?) `(,pair?))
	    ((peek-char) `(,peek-char))
	    ((positive?) `(,positive?))
	    ((procedure?) `(,procedure?))
	    ((quotient) `(,quotient))
	    ((rational?) `(,rational?))
	    ((read) `(,read))
	    ((read-char) `(,read-char))
	    ((real?) `(,real?))
	    ((remainder) `(,remainder))
	    ((reverse) `(,reverse))
	    ((round) `(,round))
	    ((set-car!) `(,set-car!))
	    ((set-cdr!) `(,set-cdr!))
	    ((string) `(,string))
	    ((string->list) `(,string->list))
	    ((string->number) `(,string->number))
	    ((string->symbol) `(,string->symbol))
	    ((string-append) `(,string-append))
	    ((string-ci<=?) `(,string-ci<=?))
	    ((string-ci<?) `(,string-ci<?))
	    ((string-ci=?) `(,string-ci=?))
	    ((string-ci>=?) `(,string-ci>=?))
	    ((string-ci>?) `(,string-ci>?))
	    ((string-length) `(,string-length))
	    ((string-ref) `(,string-ref))
	    ((string-set!) `(,string-set!))
	    ((string<=?) `(,string<=?))
	    ((string<?) `(,string<?))
	    ((string=?) `(,string=?))
	    ((string>=?) `(,string>=?))
	    ((string>?) `(,string>?))
	    ((string?) `(,string?))
	    ((substring) `(,substring))
	    ((symbol->string) `(,symbol->string))
	    ((symbol?) `(,symbol?))
	    ((truncate) `(,truncate))
	    ((vector) `(,vector))
	    ((vector->list) `(,vector->list))
	    ((vector-length) `(,vector-length))
	    ((vector-ref) `(,vector-ref))
	    ((vector-set!) `(,vector-set!))
	    ((vector?) `(,vector?))
	    ((write) `(,write))
	    ((write-char) `(,write-char))
	    ((zero?) `(,zero?))

	    ;; psd globals
	    ((psd-set-breakpoint) `(,psd-set-breakpoint))
	    ((psd-clear-breakpoint) `(,psd-clear-breakpoint))
	    ((psd-reset) `(,psd-reset))
	    ((psd-break) `(,psd-break))
	    ((*psd-stepping-by-line*) `(,*psd-stepping-by-line*))
	    ((*psd-coming-from-line*) `(,*psd-coming-from-line*))
	    ((*psd-break?*) `(,*psd-break?*))
	    ((*psd-breakpoints*) `(,*psd-breakpoints*))
	    (else #f)))))

(define psd-global-symbol-setters
  (list (lambda (psd-temp psd-temp2)
	  (case psd-temp 

	    ;; You can also put additional global symbols here.

	    ;; psd globals
	    ((psd-set-breakpoint) (set! psd-set-breakpoint psd-temp2)) 
	    ((psd-reset) (set! psd-reset psd-temp2)) 
	    ((*psd-stepping-by-line*) (set! *psd-stepping-by-line* psd-temp2)) 
	    ((*psd-coming-from-line*) (set! *psd-coming-from-line* psd-temp2)) 
	    ((*psd-break?*) (set! *psd-break?* psd-temp2)) 
	    ((*psd-breakpoints*) (set! *psd-breakpoints* psd-temp2))
	    (else #f)))))

;;;
;;; The scope is determined using a scheme similar to accessing variables.
;;; 


(define (psd-context) '())



;;;
;;; In order to be able to catch runtime type errors and calls with
;;; wrong number of arguments, each procedure call is made via
;;; psd-apply. It checks if the procedure to be applied is found in
;;; the list of primitive proceduers. If it is, the number of arguments 
;;; and their types are checked using the information in the list.
;;; The format of the list is
;;;
;;; (... (procedure (number-of-args assertion ...) ...) ...)
;;;

;;; Each sublist specifies one allowable case of number of arguments.
;;; The number of args can be a number or the symbol &rest meaning an
;;; arbitrary number of arguments. Each assertion is a procedure of
;;; one argument. It is called with a list of the values of each
;;; subexpression in the procedure call. Most of the assertions are
;;; expressed with the procedure assert, that takes the argument
;;; position to check and a predicate to apply to it. The assertions
;;; work "backward", returning false, if the call can be made. If an
;;; assertion fails (the call would result in a run time error), it
;;; returns a string to be displayed.

;;;
;;; If a run time error would occur, the debugger is invoked with an
;;; appropriate message given to the user.
;;;
;;; Bound checking is not yet done, but it might be useful for eg.
;;; list-ref, vector-ref etc.
;;;
;;; I have also cheated a bit in places like for-each, where only the
;;; first argument is checked for. I have tried to indicate these with
;;; "needs work".
;;; 

(define psd-apply

  (let ((+ +) (= =) (apply apply) (assoc assoc) (assq assq)
	      (cadr cadr) (car car) (cdr cdr) (cons cons)
	      (display display) (eq? eq?) (equal? equal?)
	      (for-each for-each) (length length)
	      (list->string list->string) (list-ref list-ref)
	      (map map) (newline newline) (not not) (null? null?)
	      (number->string number->string) (pair? pair?)
	      (reverse reverse)
	      (string-append string-append) (symbol->string symbol->string))
    (letrec

	;; Names of all the essential procedures
	((r4rs-names
	  `(
	    ;; You can add additional procedures here.

	    
	    ;; R4RS essentials
	    (,* *) 
	    (,+ +) 
	    (,- -) 
	    (,/ /) 
	    (,< <) 
	    (,<= <=) 
	    (,= =) 
	    (,> >) 
	    (,>= >=)
	    (,abs abs) 
	    (,append append) 
	    (,apply apply) 
	    (,assoc assoc) 
	    (,assq assq)
	    (,assv assv) 
	    (,boolean? boolean?) 
	    (,caaaar caaaar) 
	    (,caaadr caaadr)
	    (,caaar caaar) 
	    (,caadar caadar) 
	    (,caaddr caaddr) 
	    (,caadr caadr) 
	    (,caar caar)
	    (,cadaar cadaar) 
	    (,cadadr cadadr) 
	    (,cadar cadar) 
	    (,caddar caddar)
	    (,cadddr cadddr) 
	    (,caddr caddr) 
	    (,cadr cadr)
	    (,call-with-current-continuation call-with-current-continuation)
	    (,call-with-input-file call-with-input-file)
	    (,call-with-output-file call-with-output-file)
	    (,car car) 
	    (,cdaaar cdaaar) 
	    (,cdaadr cdaadr) 
	    (,cdaar cdaar) 
	    (,cdadar cdadar)
	    (,cdaddr cdaddr) 
	    (,cdadr cdadr) 
	    (,cdar cdar) 
	    (,cddaar cddaar) 
	    (,cddadr cddadr)
	    (,cddar cddar) 
	    (,cdddar cdddar) 
	    (,cddddr cddddr) 
	    (,cdddr cdddr) 
	    (,cddr cddr)
	    (,cdr cdr) 
	    (,ceiling ceiling) 
	    (,char->integer char->integer)
	    (,char-alphabetic? char-alphabetic?) 
	    (,char-ci<=? char-ci<=?) 
	    (,char-ci<? char-ci<?) 
	    (,char-ci=? char-ci=?) 
	    (,char-ci>=? char-ci>=?)
	    (,char-ci>? char-ci>?) 
	    (,char-downcase char-downcase) 
	    (,char-lower-case? char-lower-case?)
	    (,char-numeric? char-numeric?)
	    (,char-upcase char-upcase)
	    (,char-upper-case? char-upper-case?)
	    (,char-whitespace? char-whitespace?)
	    (,char<=? char<=?)
	    (,char<? char<?)
	    (,char=? char=?)
	    (,char>=? char>=?)
	    (,char>? char>?)
	    (,char? char?)
	    (,close-input-port close-input-port)
	    (,close-output-port close-output-port)
	    (,complex? complex?)
	    (,cons cons)
	    (,current-input-port current-input-port)
	    (,current-output-port current-output-port)
	    (,display display)
	    (,eof-object? eof-object?)
	    (,eq? eq?)
	    (,equal? equal?)
	    (,eqv? eqv?)
	    (,even? even?)
	    (,exact? exact?)
	    (,floor floor)
	    (,for-each for-each)
	    (,gcd gcd)
	    (,inexact? inexact?)
	    (,input-port? input-port?)
	    (,integer->char integer->char)
	    (,integer? integer?)
	    (,lcm lcm)
	    (,length length)
	    (,list list)
	    (,list->string list->string)
	    (,list->vector list->vector)
	    (,list-ref list-ref)
	    (,list? list?)
	    (,load load)
	    (,make-string make-string)
	    (,make-vector make-vector)
	    (,map map)
	    (,max max)
	    (,member member)
	    (,memq memq)
	    (,memv memv)
	    (,min min)
	    (,modulo modulo)
	    (,negative? negative?)
	    (,newline newline)
	    (,not not)
	    (,null? null?)
	    (,number->string number->string)
	    (,number? number?)
	    (,odd? odd?)
	    (,open-input-file open-input-file)
	    (,open-output-file open-output-file)
	    (,output-port? output-port?)
	    (,pair? pair?)
	    (,peek-char peek-char)
	    (,positive? positive?)
	    (,procedure? procedure?)
	    (,quotient quotient)
	    (,rational? rational?)
	    (,read read)
	    (,read-char read-char)
	    (,real? real?)
	    (,remainder remainder)
	    (,reverse reverse)
	    (,round round)
	    (,set-car! set-car!)
	    (,set-cdr! set-cdr!)
	    (,string string)
	    (,string->list string->list)
	    (,string->number string->number)
	    (,string->symbol string->symbol)
	    (,string-append string-append)
	    (,string-ci<=? string-ci<=?)
	    (,string-ci<? string-ci<?)
	    (,string-ci=? string-ci=?)
	    (,string-ci>=? string-ci>=?)
	    (,string-ci>? string-ci>?)
	    (,string-length string-length)
	    (,string-ref string-ref)
	    (,string-set! string-set!)
	    (,string<=? string<=?)
	    (,string<? string<?)
	    (,string=? string=?)
	    (,string>=? string>=?)
	    (,string>? string>?)
	    (,string? string?)
	    (,substring substring)
	    (,symbol->string symbol->string)
	    (,symbol? symbol?)
	    (,truncate truncate)
	    (,vector vector)
	    (,vector->list vector->list)
	    (,vector-length vector-length)
	    (,vector-ref vector-ref)
	    (,vector-set! vector-set!)
	    (,vector? vector?)
	    (,write write)
	    (,write-char write-char)
	    (,zero? zero?)))


	 ;;
	 ;; Check that the c...r operation can be made safely.
	 ;;
	 (successive-pairs
	  (lambda operations
	    (lambda (combination)
	      (let loop ((operations (reverse operations))
			 (trail '())
			 (this (cadr combination)))
		(cond ((null? operations)
		       #f)
		      ((not (pair? this))
		       (if (null? trail)
			   "argument not a pair"
			   (string-append "c"
					  (list->string (reverse trail))
					  "r of argument not a pair")))
		      (else
		       (loop (cdr operations)
			     (cons (if (eq? car (car operations))
				       #\a
				       #\d)
				   trail)
			     ((car operations) this))))))))

	 ;;
	 ;; Check the i'th position of args
	 ;;

	 (assert
	  (lambda (i predicate? . predicate-name)
	    (lambda (combination)
	      (if (equal? i 'all)
		  (let loop ((args (cdr combination))
			     (failures '())
			     (i 1))
		    (cond

		     ;; no failures
		     ((and (null? args)
			   (null? failures))
		      #f)

		     ;; there were failures
		     ((null? args)
		      (string-append (if (= (length failures) 1)
					 "argument at position "
					 "arguments at positions ")
				     (apply string-append
					    (map
					     (lambda (n)
					       (string-append
						(number->string n)
						" "))
					     (reverse failures)))
				     (if (null? predicate-name)
					 (string-append
					  "does not satisfy predicate "
					  (procedure-name predicate?))
					 (car predicate-name))))
		     (else
		      (loop (cdr args)
			    (if (predicate? (car args))
				failures
				(cons i failures))
			    (+ i 1)))))
		
		  (let ((result (predicate? (list-ref combination i))))
		    (if result
			#f
			(string-append "argument "
				       (number->string i)
				       (if (null? predicate-name)
					   (string-append
					    " does not satisfy predicate "
					    (procedure-name predicate?))
					   (car predicate-name)))))))))

	 ;;
	 ;; Check for pair or null.
	 ;;

	 (psd-list?
	  (lambda (x) (or (null? x)
			  (pair? x))))

	 ;;
	 ;; Get the name of a procedure
	 ;;

	 (procedure-name
	  (lambda (proc)
	    (let ((entry (assoc proc r4rs-names)))
	      (if entry
		  (symbol->string (cadr entry))
		  "#[unknown primitive procedure]")))))

	 (let
	     ((primitive-procedures
	       `(
		 ;; You can add additional primitive procedures here.
	      

		 ;; R4RS essentials
		 (,* (&rest ,(assert 'all number?)))
		 (,+ (&rest ,(assert 'all number?)))
		 (,- (&rest ,(assert 'all number?)))
		 (,/ (&rest ,(assert 'all number?)))
		 (,< (&rest ,(assert 'all number?)))
		 (,<= (&rest ,(assert 'all number?)))
		 (,= (&rest ,(assert 'all number?)))
		 (,> (&rest ,(assert 'all number?)))
		 (,>= (&rest ,(assert 'all number?)))
		 (,abs (1 ,(assert 1 number?)))
		 (,append (&rest ,(assert 'all psd-list?
					  " is not a list.")))
		 (,apply (&rest ,(assert 1 procedure?)))
		 (,assoc (2 ,(assert 2 psd-list? " is not a list.")))
		 (,assq (2 ,(assert 2 psd-list? " is not a list.")))
		 (,assv (2 ,(assert 2 psd-list? " is not a list.")))
		 (,boolean? (1))
		 (,caaaar (1 ,(successive-pairs car car car car)))
		 (,caaadr (1 ,(successive-pairs car car car cdr)))
		 (,caaar (1 ,(successive-pairs car car car)))
		 (,caadar (1 ,(successive-pairs car car cdr car)))
		 (,caaddr (1 ,(successive-pairs car car cdr cdr)))
		 (,caadr (1 ,(successive-pairs car car cdr)))
		 (,caar (1 ,(successive-pairs car car)))
		 (,cadaar (1 ,(successive-pairs car cdr car car)))
		 (,cadadr (1 ,(successive-pairs car cdr car cdr)))
		 (,cadar (1 ,(successive-pairs car cdr car)))
		 (,caddar (1 ,(successive-pairs car cdr cdr car)))
		 (,cadddr (1 ,(successive-pairs car cdr cdr cdr)))
		 (,caddr (1 ,(successive-pairs car cdr cdr)))
		 (,cadr (1 ,(successive-pairs car cdr)))
		 (,call-with-current-continuation (1 ,(assert 1 procedure?)))
		 (,call-with-input-file (2 ,(assert 1 string?) ,(assert 2 procedure?)))
		 (,call-with-output-file (2 ,(assert 1 string?) ,(assert 2 procedure?)))
		 (,car (1 ,(assert 1 pair?)))
		 (,cdaaar (1 ,(successive-pairs cdr car car car)))
		 (,cdaadr (1 ,(successive-pairs cdr car car cdr)))
		 (,cdaar (1 ,(successive-pairs cdr car car)))
		 (,cdadar (1 ,(successive-pairs cdr car cdr car)))
		 (,cdaddr (1 ,(successive-pairs cdr car cdr cdr)))
		 (,cdadr (1 ,(successive-pairs cdr car cdr)))
		 (,cdar (1 ,(successive-pairs cdr car)))
		 (,cddaar (1 ,(successive-pairs cdr cdr car car)))
		 (,cddadr (1 ,(successive-pairs cdr cdr car cdr)))
		 (,cddar (1 ,(successive-pairs cdr cdr car)))
		 (,cdddar (1 ,(successive-pairs cdr cdr cdr car)))
		 (,cddddr (1 ,(successive-pairs cdr cdr cdr cdr)))
		 (,cdddr (1 ,(successive-pairs cdr cdr cdr)))
		 (,cddr (1 ,(successive-pairs cdr cdr)))
		 (,cdr (1 ,(assert 1 pair?)))
		 (,ceiling (1 ,(assert 1 number?)))
		 (,char->integer (1 ,(assert 1 char?)))
		 (,char-alphabetic? (1 ,(assert 1 char?)))
		 (,char-ci<=? (&rest ,(assert 'all char?)))
		 (,char-ci<? (&rest ,(assert 'all char?)))
		 (,char-ci=? (&rest ,(assert 'all char?)))
		 (,char-ci>=? (&rest ,(assert 'all char?)))
		 (,char-ci>? (&rest ,(assert 'all char?)))
		 (,char-downcase (1 ,(assert 1 char?)))
		 (,char-lower-case? (1 ,(assert 1 char?)))
		 (,char-numeric? (1 ,(assert 1 char?)))
		 (,char-upcase (1 ,(assert 1 char?)))
		 (,char-upper-case? (1 ,(assert 1 char?)))
		 (,char-whitespace? (1 ,(assert 1 char?)))
		 (,char<=? (&rest ,(assert 'all char?)))
		 (,char<? (&rest ,(assert 'all char?)))
		 (,char=? (&rest ,(assert 'all char?)))
		 (,char>=? (&rest ,(assert 'all char?)))
		 (,char>? (&rest ,(assert 'all char?)))
		 (,char? (1))
		 (,close-input-port (1 ,(assert 1 input-port?)))
		 (,close-output-port (1 ,(assert 1 output-port?)))
		 (,complex? (1))
		 (,cons (2))
		 (,current-input-port (0))
		 (,current-output-port (0))
		 (,display (1) (2 ,(assert 2 output-port?)))
		 (,eof-object? (1))
		 (,eq? (2))
		 (,equal? (2))
		 (,eqv? (2))
		 (,even? (1 ,(assert 1 integer?)))
		 (,exact? (1 ,(assert 1 number?)))
		 (,floor (1 ,(assert 1 number?)))
		 (,for-each (&rest ,(assert 1 procedure?))) ; needs work
		 (,gcd (&rest ,(assert 'all integer?)))
		 (,inexact? (1 ,(assert 1 number?)))
		 (,input-port? (1))
		 (,integer->char (1 ,(assert 1 integer?)))
		 (,integer? (1))
		 (,lcm (&rest ,(assert 'all integer?)))
		 (,length (1 ,(assert 1 psd-list? "is not a list.")))
		 (,list (&rest))
		 (,list->string (1 ,(assert 1 psd-list? " is not a list."))) ; needs work
		 (,list->vector (1 ,(assert 1 psd-list? " is not a list.")))
		 (,list-ref (2 ,(assert 1 psd-list? " is not a list.") ,(assert 2 integer?)))
		 (,list? (1))
		 (,load (1 ,(assert 1 string?)))
		 (,make-string (1 ,(assert 1 integer?)) (2 ,(assert 1 integer?) ,(assert 2 char?)))
		 (,make-vector (1 ,(assert 1 integer?)) (2 ,(assert 1 integer?)))
		 (,map (&rest ,(assert 1 procedure?)))
		 (,max (&rest ,(assert 'all number?)))
		 (,member (2 ,(assert 2 psd-list? " is not a list.")))
		 (,memq (2 ,(assert 2 psd-list? " is not a list.")))
		 (,memv (2 ,(assert 2 psd-list? " is not a list.")))
		 (,min (&rest ,(assert 'all number?)))
		 (,modulo (2 ,(assert 'all integer?)))
		 (,negative? (1 ,(assert 1 number?)))
		 (,newline (0) (1 ,(assert 1 output-port?)))
		 (,not (1))
		 (,null? (1))
		 (,number->string
		  (1 ,(assert 1 number?))
		  (2 ,(assert 1 number?)
		     ,(assert 2 (lambda (n) (member n '(2 8 10 16)))
			      "base must be either 2, 8, 10 or 16")))
		 (,number? (1))
		 (,odd? (1 ,(assert 1 integer?)))
		 (,open-input-file (1 ,(assert 1 string?)))
		 (,open-output-file (1 ,(assert 1 string?)))
		 (,output-port? (1))
		 (,pair? (1))
		 (,peek-char (0) (1 ,(assert 1 input-port?)))
		 (,positive? (1 ,(assert 1 number?)))
		 (,procedure? (1))
		 (,quotient (2 ,(assert 'all integer?)))
		 (,rational? (1))
		 (,read (0) (1 ,(assert 1 input-port?)))
		 (,read-char (0) (1 ,(assert 1 input-port?)))
		 (,real? (1))
		 (,remainder (2 ,(assert 'all integer?)))
		 (,reverse (1 ,(assert 1 psd-list? " is not a list.")))
		 (,round (1 ,(assert 1 number?)))
		 (,set-car! (2 ,(assert 1 pair?)))
		 (,set-cdr! (2 ,(assert 1 pair?)))
		 (,string (&rest ,(assert 'all char?)))
		 (,string->list (1 ,(assert 1 string?)))
		 (,string->number
		  (1 ,(assert 1 string?))
		  (2 ,(assert 1 string?)
		     ,(assert 2 (lambda (n) (member n '(2 8 10 16)))
			      "base must be either 2, 8, 10 or 16")))
		 (,string->symbol (1 ,(assert 1 string?)))
		 (,string-append (&rest ,(assert 'all string?)))
		 (,string-ci<=? (&rest ,(assert 'all string?)))
		 (,string-ci<? (&rest ,(assert 'all string?)))
		 (,string-ci=? (&rest ,(assert 'all string?)))
		 (,string-ci>=? (&rest ,(assert 'all string?)))
		 (,string-ci>? (&rest ,(assert 'all string?)))
		 (,string-length (1 ,(assert 1 string?)))
		 (,string-ref (2 ,(assert 1 string?) ,(assert 2 integer?)))
		 (,string-set! (3 ,(assert 1 string?) ,(assert 2 integer?) ,(assert 3 char?)))
		 (,string<=? (&rest ,(assert 'all string?)))
		 (,string<? (&rest ,(assert 'all string?)))
		 (,string=? (&rest ,(assert 'all string?)))
		 (,string>=? (&rest ,(assert 'all string?)))
		 (,string>? (&rest ,(assert 'all string?)))
		 (,string? (1))
		 (,substring (3 ,(assert 1 string?) ,(assert 2 integer?) ,(assert 3 integer?)))
		 (,symbol->string (1 ,(assert 1 symbol?)))
		 (,symbol? (1))
		 (,truncate (1 ,(assert 1 number?)))
		 (,vector (&rest))
		 (,vector->list (1 ,(assert 1 vector?)))
		 (,vector-length (1 ,(assert 1 vector?)))
		 (,vector-ref (2 ,(assert 1 vector?) ,(assert 2 integer?)))
		 (,vector-set! (3 ,(assert 1 vector?) ,(assert 2 integer?)))
		 (,vector? (1))
		 (,write (1) (2 ,(assert 2 output-port?)))
		 (,write-char (1) (2 ,(assert 2 output-port?)))
		 (,zero? (1 ,(assert 1 number?)) (name zero?)))))

	   (lambda (combination . debug-arguments)
	     (if (assq (car combination) primitive-procedures)


		 ;; This is a primitive, check number of args and their types
		 (let ((entry (cdr (assq (car combination)
					 primitive-procedures))))


		   ;; See if there is a subentry for the number of arguments
		   ;; in the call.
		   (let ((subentry (or (assoc (length (cdr combination))
					      entry)
				       (assoc '&rest
					      entry))))

		     ;; Check the number of arguments
		     (if (not subentry)

			 ;; There was no subentry for this number of arguments
			 (begin
			   (display "ERROR: Wrong number of arguments to primitive procedure ")
			   (display (procedure-name (car combination)))
			   (newline)
			   (apply psd-debug debug-arguments))

			 ;; There was a subentry, check that all the
			 ;; assertions given in it are satisfied before
			 ;; doing the procedure call
			 (begin
			   (let loop ((assertions (cdr subentry))
				      (errors '()))
			     (cond

			      ;; there were no type errors
			      ((and (null? assertions)
				    (null? errors))
			       (apply (car combination)
				      (cdr combination)))

			      ;; all assertions checked, there were type errors
			      ((null? assertions)
			       (display "ERROR: Type error when calling primitive procedure ")
			       (display (procedure-name (car combination)))
			       (newline)
			       (for-each (lambda (str)
					   (display "       ")
					   (display str)
					   (newline))
					 (reverse errors))
			       (apply psd-debug debug-arguments))

			      ;; still more to check
			      (else
			       (loop (cdr assertions)
				     (let ((result ((car assertions) combination)))
				       (if result
					   (cons result
						 errors)
					   errors))))))))))

		 (if (procedure? (car combination))
		     
		     ;; This is just a normal user procedure or a non-essential
		     ;; primitive. Cross your fingers and go!
		     (apply (car combination)
			    (cdr combination))

		     ;; This is not a procedure at all!
		     (begin
		       (display "ERROR: Attempt to call a non procedural object ")
		       (display (car combination))
		       (newline)
		       (apply psd-debug debug-arguments)))))))))

	  
