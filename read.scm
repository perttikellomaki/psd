;;;;
;;;; $Id: read.scm,v 1.5 1993/09/30 07:41:51 pk Exp $
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
;;;; This file contains the reader for psd. We can not use plain read,
;;;; because we want to know where in a file we are. The reader
;;;; returns a pexp, which is a sexp with position information.
;;;;

;;;;
;;;; $Log: read.scm,v $
;;;; Revision 1.5  1993/09/30  07:41:51  pk
;;;; Now accepts #d syntax for decimal numbers. Patch by Aubrey Jaffer.
;;;;
;;;; Revision 1.4  1993/09/24  07:58:00  pk
;;;; Changed version number to 1.1.
;;;;
;;;; Revision 1.3  1993/09/06  15:08:48  pk
;;;; Added the procedure psd-reset-read.
;;;;
;;;; Revision 1.2  1993/05/18  11:30:37  pk
;;;; Added #\tab to character names.
;;;;
;;;; Revision 1.1  1992/12/07  11:05:24  pk
;;;; Initial revision
;;;;
;;;;
;;;; Moved to RCS from sccs Dec 7th 1992. The SCCS log:
;;;; SCCS/s.read.scm:
;;;; 
;;;; D 1.22	92/10/02 15:59:33 pk	22 21	00029/00003/00664
;;;; Added detection of the implementation preferred case. Now works with sci.
;;;; 
;;;; D 1.21	92/10/02 09:20:02 pk	21 20	00009/00010/00658
;;;; Removed error from the procedures that are closed over. In MIT Scheme
;;;; error is a special form, and this caused problems.
;;;; 
;;;; D 1.20	92/10/02 09:10:39 pk	20 19	00035/00023/00633
;;;; Replaced error with psd-error.
;;;; 
;;;; D 1.19	92/07/16 14:18:39 pk	19 18	00001/00001/00655
;;;; Removed garbage that was left accidentally after applying
;;;; Joerg Leisenberg's patches.
;;;; 
;;;; D 1.18	92/07/16 14:13:37 pk	18 17	00006/00003/00650
;;;; Moved eof-object? test to be the first test in internal-read.
;;;; Suggested by Joerg Leisenberg, needed for MIT Scheme.
;;;; 
;;;; D 1.17	92/07/09 13:41:17 pk	17 16	00022/00008/00631
;;;; Added support for float with exponent.
;;;; 
;;;; D 1.16	92/07/09 13:16:56 pk	16 15	00031/00010/00608
;;;; Added support for vector constants.
;;;; 
;;;; D 1.15	92/07/09 11:10:45 pk	15 14	00001/00001/00617
;;;; Changed the version number from 0.99 to 1.0
;;;; Hope this was not too early!
;;;; 
;;;; D 1.14	92/07/09 11:00:37 pk	14 13	00146/00146/00472
;;;; Changed the names pcar, pcdr etc. to psd-car, psd-cdr etc.
;;;; It does not look nice, but this way the only visible names start with
;;;; psd- or *psd-. This is important.
;;;; 
;;;; D 1.13	92/07/08 14:12:28 pk	13 12	00003/00002/00615
;;;; Fixed psd-path->index.
;;;; 
;;;; D 1.12	92/07/07 11:48:54 pk	12 11	00001/00001/00616
;;;; Removed get-char from the closed-in variables of psd-read.
;;;; 
;;;; D 1.11	92/07/06 17:03:20 pk	11 10	00397/00383/00220
;;;; Made most of the procedures in psd closures that contain the original
;;;; values of the primitive procedures. This way, one can redefine cons,
;;;; car etc. without affecting the debugger.
;;;; 
;;;; D 1.10	92/07/06 15:06:04 pk	10 9	00044/00013/00559
;;;; Changed source-line-number and source-char-position to
;;;; *psd-...*
;;;; 
;;;; Added psd-path->index and psd-index->path that associate each file
;;;; name with an integer. It is only the integer that is written in the
;;;; instrumented source code.
;;;; 
;;;; D 1.9	92/06/30 11:57:46 pk	9 8	00046/00020/00526
;;;; Merged in hex, octal and binary support by Edward Briggs
;;;; (briggs@getoff.dec.com). 
;;;; 
;;;; D 1.8	92/06/29 10:24:34 pk	8 7	00036/00000/00510
;;;; Added hex support by Edward Briggs (briggs@getoff.nyo.dec.com).
;;;; 
;;;; D 1.7	92/06/26 16:48:58 pk	7 6	00001/00001/00509
;;;; Added version number.
;;;; 
;;;; D 1.6	92/06/26 09:52:09 pk	6 5	00049/00000/00461
;;;; Added quasiquote, unquote and unquote splicing.
;;;; 
;;;; D 1.5	92/06/25 16:04:05 pk	5 4	00001/00001/00460
;;;; More iso latin stuff.
;;;; 
;;;; D 1.4	92/06/25 16:01:51 pk	4 3	00001/00001/00460
;;;; Changed the iso latin \"a to a.
;;;; 
;;;; D 1.3	92/06/25 12:28:37 pk	3 2	00157/00111/00304
;;;; Added simple float support for numbers like 1.23
;;;; 
;;;; D 1.2	92/06/23 12:30:50 pk	2 1	00120/00067/00295
;;;; No major changes.
;;;; 
;;;; D 1.1	92/05/27 10:23:05 pk	1 0	00362/00000/00000
;;;; date and time created 92/05/27 10:23:05 by pk
;;;; 

;;;----------------------------------------------------------------------
;;; modification: egb (edward briggs (briggs@getoff.dec.com)) added support 
;;                for binary, octal and hex numbers. (e.g. #b0101, #o77, #xa0).
;;              1) added predicates digit-2? digit-8? digit-16?
;;              2) added routines read-hex-number, read binary-number, and
;;                 read-octal-number 
;;              3) added lines to read-hashed-token to find these numbers
;;
;;----------------------------------------------------------------------

;;; Current position in the source file. 

(define *psd-source-line-number* 1)
(define *psd-source-char-position* 1)

;;;
;;; Reset the position of psd-read.
;;;

(define (psd-reset-read)
  (set! *psd-source-line-number* 1)
  (set! *psd-source-char-position* 1))

;;;
;;; The tab character.
;;;

(define *psd-tab-char* (integer->char 9))
(define *psd-tab-width* 8)

;;;
;;; Are symbols converted to upper case, lower case or not converted?
;;; 

(define *psd-preferred-case*
  (cond ((string=? (symbol->string 'Foo) "foo")
	 'lowercase)
	((string=? (symbol->string 'Foo) "FOO")
	 'uppercase)
	(else 'original-case)))

;;; In order to save space, path names are stored as integers in the
;;; instrumented file. psd-path->index and psd-index->path do the
;;; conversion.

(define psd-path->index #f)
(define psd-index->path #f)

(let ((path-names '())
      (count -1))
  
  (set! psd-path->index
	(lambda (str)
	  (let ((result (assoc str path-names)))
	    (if (not result)
		(begin
		  (set! count (+ count 1))
		  (set! path-names
			`((,count . ,str)
			  (,str . ,count)
			  ,@path-names))
		  count)
		(cdr result)))))
  
  (set! psd-index->path
	(lambda (index)
	  (cdr (assoc index path-names)))))

;;;
;;; Read an expression from the port, and tag it with the given source
;;; file name and position information.


(define psd-read
  
  (let ((+ +) (- -) (= =) (boolean? boolean?) (caddr caddr) (cadr cadr)
	      (car car) (cddr cddr) (cdr cdr)
	      (char-whitespace? char-whitespace?) (char=? char=?)
	      (char? char?) (cons cons) (eof-object? eof-object?)
	      (eq? eq?) (equal? equal?)
	      (length length) (list list) (list->string list->string)
	      (member member) (not not) (null? null?) (number? number?)
	      (peek-char peek-char) (read read) (read-char read-char)
	      (reverse reverse) (string->number string->number)
	      (string->symbol string->symbol) (string-append string-append)
	      (string-ci=? string-ci=?) (string? string?) (symbol? symbol?))
    
    (lambda (port source-file-name)

;;;
;;; Read a character and update position.
;;;
      
      (define (get-char)
	(let ((char (read-char port)))
	  (cond ((eof-object? char) char)
		((char=? char #\newline)
		 (set! *psd-source-char-position* 1)
		 (set! *psd-source-line-number* (+ *psd-source-line-number* 1)))
		((char=? char *psd-tab-char*)
		 (set! *psd-source-char-position*
		       (+ (* *psd-tab-width*
			     (+ (quotient *psd-source-char-position*
					  *psd-tab-width*)
			     1))
			  1)))
		(else
		 (set! *psd-source-char-position* (+ *psd-source-char-position* 1))))
	  char))
      
;;;
;;; Look at the next character.
;;;
      
      (define (next-char) (peek-char port))
      
;;;
;;; Is the next character one of the given ones?
;;;
      
      (define (next? . chars)
	(member (next-char) chars))
      
;;;
;;; Build a list describing the current position
;;;
      
      (define (current-position)
	(list (psd-path->index source-file-name)
	      *psd-source-line-number*
	      *psd-source-char-position*))
;;;
;;; Tokens. The starting and ending positions are supplied with
;;; each token.
;;;
      
      (define (make-token start end contents) (list start end contents))
      (define (token-start tok) (car tok))
      (define (token-end tok) (cadr tok))
      (define (token-contents tok) (caddr tok))
      
;;;
;;; These are used for some special tokens.
;;;
      
      (define left-paren '(left-paren))
      (define right-paren '(right-paren))
      (define vector-start '(vector-start))
      (define dot '(dot))
      (define quote-token '(quote))
      (define quasiquote-token '(quasiquote))
      (define unquote-token '(unquote))
      (define unquote-splicing-token '(unquote-splicing))
      (define line-directive-token '(line-directive))
      
;;;
;;; Classify characters. See R4RS Formal syntax (7.1)
;;;
      
      (define (letter? c)
	(member c '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n
			#\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\A
			#\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N
			#\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z)))
      
      (define (special-initial? c)
	(member c '(#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\~ #\_ #\^)))
      
      (define (initial? c)
	(or (letter? c) (special-initial? c)))
      
      (define (digit? c)
	(member c '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0)))
      
      (define (digit-2? c)
	(member c '(#\0 #\1)))
      
      (define (digit-8? c)
	(member c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)))
      
      (define (digit-16? c)
	(member c '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0 #\a #\b
			#\c #\d #\e #\f #\A #\B #\C #\D #\E #\F)))
      
      (define (special-subsequent? c)
	(member c '(#\. #\+ #\- )))
      
      (define (subsequent? c)
	(or (initial? c) (digit? c) (special-subsequent? c)))
      
;;;
;;; Skip white space.
;;;
      
      (define (skip-white-space)
	(if (eof-object? (next-char))
	    #f
	    (cond
	     ((char-whitespace? (next-char))
	      (get-char)
	      (skip-white-space))
	     ((next? #\;)
	      (let loop ()
		(cond ((eof-object? (next-char))
		       #f)
		      ((next? #\newline)
		       (skip-white-space))
		      (else
		       (get-char)
		       (loop))))))))
      
      
;;;
;;; Read next token.
;;;
      
      (define (read-token)
	(skip-white-space)
	(if (equal? (next-char) #\#)
	    
	    ;; If it starts with a hash sign, it might be a line
	    ;; directive. In that case, just read the next token.
	    (let* ((start (current-position))
		   (contents (read-hashed-token))
		   (end (current-position)))
	      (if (eq? contents line-directive-token)
		  (read-token)
		  (make-token start end contents)))
	    
	    (let* ((start (current-position))
		   (contents
		    (cond
		     ((eof-object? (next-char))
		      (get-char))
		     ((initial? (next-char))
		      (read-identifier))
		     ((next? #\+ #\- #\.)
		      (maybe-read-peculiar-identifier))
		     ((digit? (next-char))
		      (read-number))
		     ((next? #\()
		      (get-char)
		      left-paren)
		     ((next? #\))
		      (get-char)
		      right-paren)
		     ((next? #\')
		      (get-char)
		      quote-token)
		     ((next? #\`)
		      (get-char)
		      quasiquote-token)
		     ((next? #\,)
		      (get-char)
		      (if (next? #\@)
			  (begin (get-char)
				 unquote-splicing-token)
			  unquote-token))
		     ((next? #\")
		      (read-string))
		     (else
		      (error "read-token: bad character " (next-char)))))
		   (end (current-position)))
	      (make-token start end contents))))
      
;;;
;;; Read a string.
;;;
      
      (define (read-string)
	(get-char)
	(let loop ((result '()))
	  (cond
	   ((next? #\")
	    (get-char)
	    (list->string (reverse result)))
	   ((next? #\\)
	    (get-char)
	    (loop (cons (get-char) result)))
	   (else
	    (loop (cons (get-char) result))))))
      
;;;
;;; Read a token starting with a hash sign.
;;;
      
      (define (read-hashed-token)
	(get-char)
	(cond
	 ((next? #\t)
	  (get-char)
	  #t)
	 ((next? #\f)
	  (get-char)
	  #f)
	 ((next? #\\)
	  (read-character))
	 ((or (next? #\x) (next? #\X))
	  (get-char)
	  (read-hex-number))
	 ((or (next? #\d) (next? #\D))
	  (get-char)
	  (read-number))
	 ((or (next? #\o) (next? #\O))
	  (get-char)
	  (read-octal-number))
	 ((or (next? #\b) (next? #\B))
	  (get-char)
	  (read-binary-number))

	 ((next? #\()
	  (get-char)
	  vector-start)

	 ;; we return a special token to inform that this was not a real
	 ;; token but a line directive
	 ((next? #\l)
	  (read-line-directive)
	  line-directive-token)
	 
	 (else
	  (error "read-hashed-token: bad character " (next-char)))))
      
      
;;;
;;; Read a line directive, of the form "#line file line column #".
;;; The trailing hash is used for making sure that we don't run past
;;; the end of line. At least scm version 3c8 will read one more trailing
;;; whitespace character than R4RS says it should. In later versions
;;; this is fixed.
;;;
      
      (define (read-line-directive)
	(get-char)
	(if (next? #\i)
	    (get-char)
	    (error "read-line-directive: bad character " (next-char)))
	(if (next? #\n)
	    (get-char)
	    (error "read-line-directive: bad character " (next-char)))
	(if (next? #\e)
	    (get-char)
	    (error "read-line-directive: bad character " (next-char)))
	
	;; now we don't have to worry about loosing count where we are,
	;; because we are going to read the new position from the file.
	(set! source-file-name (read port))
	(set! *psd-source-line-number* (read port))
	(set! *psd-source-char-position* (read port))
	
	;; the position corresponds to the start of next line
	(let loop ((next (read-char port)))
	  (if (char=? next #\newline)
	      #f
	      (loop (read-char port)))))
      
;;;
;;; Read a character constant.
;;;
      
      (define (read-character)
	(get-char)
	(let loop ((result (list (get-char))))
	  (if (letter? (next-char))
	      (loop (cons (get-char) result))
	      (cond ((= (length result) 1)
		     (car result))
		    (else
		     (let ((name (list->string (reverse result))))
		       (cond ((string-ci=? name "space") #\space)
			     ((string-ci=? name "newline") #\newline)

			     ;; #\tab is not defined in R4RS, should
			     ;; warn about that
			     ((string-ci=? name "tab") *psd-tab-char*)
			     
			     (else (error "read-character: character name not defined in R4RS "
					  name)))))))))
      

;;;
;;; Read a vector constant.
;;;

      (define (read-vector start-token)
	(let loop ((contents '())
		   (this (internal-read)))
	  (cond ((eof-object? this)
		 (error "read-vector: premature end of file"))
		((eq? (psd-expr-type this) 'right-paren)
		 (psd-make-vector (psd-expr-start start-token)
				  (psd-expr-end this)
				  (reverse contents)))
		(else (loop (cons this contents)
			    (internal-read))))))

;;;
;;; Read a normal identifier.
;;;
      
      (define (read-identifier)

	(define (toupper lst)
	  (map char-upcase lst))

	(define (tolower lst)
	  (map char-downcase lst))
	  
	(let loop ((result (list (get-char))))
	  (if (subsequent? (next-char))
	      (loop (cons (get-char) result))
	      
	      ;; this is probably not very good style, but I could not resist... -pk-
	      (string->symbol
	       (list->string
		((case *psd-preferred-case*
		   ((uppercase) toupper)
		   ((lowercase) tolower)
		   (else (lambda (x) x)))
		 (reverse result)))))))
    
;;;
;;; Read a peculiar identifier (+ - ... or a single dot)
;;; 
      
      (define (maybe-read-peculiar-identifier)
	(let ((first (get-char)))
	  (case first
	    ((#\+)
	     (if (digit? (next-char))
		 (read-number)
		 '+))
	    ((#\-)
	     (if (digit? (next-char))
		 (- (read-number))
		 '-))
	    ((#\.)
	     (if (next? #\.)
		 (if (and (get-char)
			  (next? #\.)
			  (get-char))
		     '...
		     (error "The only identifier that may start with dot is ..."))
		 dot)))))
      
;;;
;;; Read a number. Handles only integers and floats without exponents.
;;;
      
      (define (read-number)
	
	(define (read-sign)
	  (cond ((or (next? #\+)
		     (next? #\-))
		 (string (get-char)))
		(else "")))
	
	(define (uinteger)
	  (let loop ((result '()))
	    (if (or (digit? (next-char))
		    (next? #\#))
		(loop (cons (get-char) result))
		(list->string (reverse result)))))

	(define (exponent-marker)
	  (cond ((or (next? #\e)
		     (next? #\s)
		     (next? #\f)
		     (next? #\d)
		     (next? #\l))
		 (string (get-char)))
		(else "")))
	
	(let* ((sign (read-sign))
	       (integer-part (uinteger))
	       (fractional-part
		(if (next? #\.)
		    (begin
		      (get-char)
		      (string-append "." (uinteger)))
		    ""))
	       (marker (exponent-marker))
	       (exponent
		(if (string=? "" marker)
		    ""
		    (string-append marker(uinteger)))))
	  
	  (string->number (string-append sign
					 integer-part
					 fractional-part
					 exponent))))
      
      
;;;
;;; Support for hex, octal and binary.
;;; Added by egb.
;;; 
      
      (define (read-binary-number)
	(define (binaryinteger)
	  (let loop ((result '()))
	    (if (digit-2? (next-char))
		(loop (cons (get-char) result))
		(list->string (reverse result)))))
	(string->number (string-append "#b" (binaryinteger))))
      
      (define (read-octal-number)
	(define (octalinteger)
	  (let loop ((result '()))
	    (if (digit-8? (next-char))
		(loop (cons (get-char) result))
		(list->string (reverse result)))))
	(string->number (string-append "#o" (octalinteger))))
      
      
      (define (read-hex-number)
	(define (hexinteger)
	  (let loop ((result '()))
	    (if (digit-16? (next-char))
		(loop (cons (get-char) result))
		(list->string (reverse result)))))
	
	(string->number (string-append "#x" (hexinteger))))
      
      
;;;
;;; Read a list up to the ending paren.
;;;
      
      (define (read-list starting-paren)
	
	(define (list->plist lst start end)
	  (cond
	   
	   ;; end of list
	   ((null? lst)
	    (psd-make-null start end))
	   
	   ;; dotted pair, there should be exactly one expression after the dot
	   ((eq? (psd-expr-type (car lst)) 'dot)
	    (cond ((or (null? (cdr lst))
		       (not (null? (cddr lst))))
		   (error "Bad dotted pair."))
		  (else (cadr lst))))
	   (else
	    (psd-cons (car lst)
		      (list->plist (cdr lst)
				   (if (null? (cdr lst))
				       end
				       (psd-expr-start (cadr lst)))
				   end)
		      start
		      end))))
	
	
	
	(let loop ((result '())
		   (this (internal-read)))
	  (cond
	   
	   ;; the list ended
	   ((eq? (psd-expr-type this) 'right-paren)
	    (list->plist (reverse result)
			 (token-start starting-paren)
			 (token-end this)))
	   
	   ;; continue reading
	   (else
	    (loop (cons this result)
		  (internal-read))))))
      
      
      
      
;;;
;;; The reader proper.
;;;
      
      (define (internal-read)
	(let* ((token (read-token))
	       (contents (token-contents token)))
	  (cond

	   ((eof-object? contents) ;;; check this first! 11-Jul-1992 jgl
	    contents) 
	   
	   ((eq? contents left-paren)
	    (read-list token))
	   ((eq? contents vector-start)
	    (read-vector token))
	   ((symbol? contents)
	    (psd-make-symbol
	     (token-start token)
	     (token-end token)
	     contents))
	   ((number? contents)
	    (psd-make-number
	     (token-start token)
	     (token-end token)
	     contents))
	   ((char? contents)
	    (psd-make-char
	     (token-start token)
	     (token-end token)
	     contents))
	   ((eq? contents right-paren)
	    (psd-make-expr 'right-paren
			   (token-start token)
			   (token-end token)
			   contents))
	   ((eq? contents dot)
	    (psd-make-expr 'dot
			   (token-start token)
			   (token-end token)
			   contents))
	   ((eq? contents quote-token)
	    (let ((quoted-expr (internal-read)))
	      (psd-cons (psd-make-symbol (token-start token)
					 (token-end token)
					 'quote)
			(psd-cons quoted-expr
				  (psd-make-null (psd-expr-end quoted-expr)
						 (psd-expr-end quoted-expr))
				  (psd-expr-start quoted-expr)
				  (psd-expr-end quoted-expr))
			(token-start token)
			(psd-expr-end quoted-expr))))
	   ((eq? contents quasiquote-token)
	    (let ((quasiquoted-expr (internal-read)))
	      (psd-cons (psd-make-symbol (token-start token)
					 (token-end token)
					 'quasiquote)
			(psd-cons quasiquoted-expr
				  (psd-make-null (psd-expr-end quasiquoted-expr)
						 (psd-expr-end quasiquoted-expr))
				  (psd-expr-start quasiquoted-expr)
				  (psd-expr-end quasiquoted-expr))
			(token-start token)
			(psd-expr-end quasiquoted-expr))))
	   ((eq? contents unquote-token)
	    (let ((unquoted-expr (internal-read)))
	      (psd-cons (psd-make-symbol (token-start token)
					 (token-end token)
					 'unquote)
			(psd-cons unquoted-expr
				  (psd-make-null (psd-expr-end unquoted-expr)
						 (psd-expr-end unquoted-expr))
				  (psd-expr-start unquoted-expr)
				  (psd-expr-end unquoted-expr))
			(token-start token)
			(psd-expr-end unquoted-expr))))
	   ((eq? contents unquote-splicing-token)
	    (let ((unquoted-expr (internal-read)))
	      (psd-cons (psd-make-symbol (token-start token)
					 (token-end token)
					 'unquote-splicing)
			(psd-cons unquoted-expr
				  (psd-make-null (psd-expr-end unquoted-expr)
						 (psd-expr-end unquoted-expr))
				  (psd-expr-start unquoted-expr)
				  (psd-expr-end unquoted-expr))
			(token-start token)
			(psd-expr-end unquoted-expr))))
	   
	   ((boolean? contents)
	    (psd-make-boolean
	     (token-start token)
	     (token-end token)
	     contents))
	   ((string? contents)
	    (psd-make-string
	     (token-start token)
	     (token-end token)
	     contents))
	   )))
      
      
      ;; body of psd-read
      (internal-read))))
