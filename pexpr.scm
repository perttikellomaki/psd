;;;;
;;;; $Id: pexpr.scm,v 1.2 1993/09/24 07:57:51 pk Exp $
;;;;
;;;; psd -- a portable Scheme debugger, version 1.1
;;;; Copyright (C) 1992 Pertti Kellomaki, pk@cs.tut.fi

;;;; 
;;;; Written by Pertti Kellomaki, pk@cs.tut.fi
;;;;
;;;; This file contains the implementation of pexps, which are sexps
;;;; with position information.
;;;;

;;;;
;;;; $Log: pexpr.scm,v $
;;;; Revision 1.2  1993/09/24  07:57:51  pk
;;;; Changed version number to 1.1.
;;;;
;;;; Revision 1.1  1992/12/07  10:26:31  pk
;;;; Moved to RCS.
;;;;
;;;;
;;;; Moved to RCS from SCCS Dec 7th 1992. The SCCS log:
;;;; 
;;;; SCCS/s.pexpr.scm:
;;;; 
;;;; D 1.12	92/10/02 09:19:55 pk	12 11	00024/00002/00164
;;;; Removed error from the procedures that are closed over. In MIT Scheme
;;;; error is a special form, and this caused problems.
;;;;
;;;; D 1.11	92/07/09 13:16:38 pk	11 10	00014/00010/00152
;;;; Added support for vector constants.
;;;; 
;;;; D 1.10	92/07/09 11:10:07 pk	10 9	00001/00001/00161
;;;; Changed the version number from 0.99 to 1.0
;;;; Hope this was not too early!
;;;; 
;;;; D 1.9	92/07/09 11:00:31 pk	9 8	00058/00058/00104
;;;; Changed the names pcar, pcdr etc. to psd-car, psd-cdr etc.
;;;; It does not look nice, but this way the only visible names start with
;;;; psd- or *psd-. This is important.
;;;; 
;;;; D 1.8	92/07/07 12:26:50 pk	8 7	00020/00020/00142
;;;; Fixed some misplaced parenthesis caused by using a keyboard macro in
;;;; the previous edit.
;;;; 
;;;; D 1.7	92/07/06 17:03:08 pk	7 6	00094/00046/00068
;;;; Made most of the procedures in psd closures that contain the original
;;;; values of the primitive procedures. This way, one can redefine cons,
;;;; car etc. without affecting the debugger.
;;;; 
;;;; D 1.6	92/06/26 16:48:08 pk	6 5	00001/00001/00113
;;;; Added version number.
;;;; 
;;;; D 1.5	92/06/25 16:03:28 pk	5 4	00001/00001/00113
;;;; More iso latin stuff.
;;;; 
;;;; D 1.4	92/06/25 16:01:03 pk	4 3	00001/00001/00113
;;;; Changed the iso latin \"a to a.
;;;; 
;;;; D 1.3	92/06/24 13:16:35 pk	3 2	00002/00001/00112
;;;; Added eof-object to pexp->sexp.
;;;; 
;;;; D 1.2	92/06/23 12:29:53 pk	2 1	00019/00000/00094
;;;; Now handles most special forms and internal defines.
;;;; 
;;;; D 1.1	92/05/27 10:23:31 pk	1 0	00094/00000/00000
;;;; date and time created 92/05/27 10:23:31 by pk
;;;; 


;;;
;;; Expressions. Each expression carries its starting and ending
;;; position and type tag.
;;; 

(define psd-make-expr
  (let ((list list))
    (lambda (type start end contents) (list type start end contents))))
(define psd-expr-type
  (let ((car car))
    (lambda (expr) (car expr))))
(define psd-expr-start
  (let ((cadr cadr))
    (lambda (expr) (cadr expr))))
(define psd-expr-end
  (let ((caddr caddr))
    (lambda (expr) (caddr expr))))
(define psd-expr-contents
  (let ((cadddr cadddr))
    (lambda (expr) (cadddr expr))))

;;;
;;; Easier access to the line, column and file of an expression.
;;;

(define psd-expr-start-file
  (let ((car car))
    (lambda (expr) (psd-index->path (car (psd-expr-start expr))))))
(define psd-expr-start-line
  (let ((cadr cadr))
    (lambda (expr) (cadr (psd-expr-start expr)))))
(define psd-expr-start-column
  (let ((caddr caddr))
    (lambda (expr) (caddr (psd-expr-start expr)))))
(define psd-expr-file
  (let ((car car))
    (lambda (expr) (psd-index->path (car (psd-expr-end expr))))))
(define psd-expr-end-line
  (let ((cadr cadr))
    (lambda (expr) (cadr (psd-expr-end expr)))))
(define psd-expr-end-column
  (let ((caddr caddr))
    (lambda (expr) (caddr (psd-expr-end expr)))))

;;;
;;; psd-cons, psd-car, psd-cdr, etc. work the same way as cons, car, cdr etc.,
;;; just with pexps instead of sexps.
;;;

(define psd-make-null (lambda (start end) (psd-make-expr 'null start end '())))

(define psd-null?
  (let ((eq? eq?))
    (lambda (x) (eq? 'null (psd-expr-type x)))))

(define psd-cons
  (let ((cons cons))
    (lambda (x y start end)
      (psd-make-expr 'pair start end (cons x y)))))

(define psd-pair?
  (let ((eq? eq?))
    (lambda (x) (eq? 'pair (psd-expr-type x)))))

(define psd-car
  (let ((car car))
    (lambda (x)
      (if (psd-pair? x)
	  (car (psd-expr-contents x))
	  (error "psd-car: argument not a psd-pair " x)))))

(define psd-cdr
  (let ((cdr cdr))
    (lambda (x)
      (if (psd-pair? x)
	  (cdr (psd-expr-contents x))
	  (error "psd-cdr: argument not a psd-pair " x)))))

(define psd-cadr (lambda (x) (psd-car (psd-cdr x))))
(define psd-caddr (lambda (x) (psd-cadr (psd-cdr x))))
(define psd-cddr (lambda (x) (psd-cdr (psd-cdr x))))
(define psd-cdddr (lambda (x) (psd-cdr (psd-cddr x))))
(define psd-caar (lambda (x) (psd-car (psd-car x))))
(define psd-cadar (lambda (x) (psd-car (psd-cdr (psd-car x)))))


;;;
;;; Atoms.
;;; 

(define psd-make-symbol
  (lambda (start end sym)
    (psd-make-expr 'symbol start end sym)))
(define psd-symbol?
  (let ((eq? eq?))
    (lambda (x) (eq? 'symbol (psd-expr-type x)))))
(define psd-make-number
  (lambda (start end num)
    (psd-make-expr 'number start end num)))
(define psd-number?
  (let ((eq? eq?))
    (lambda (x) (eq? 'number (psd-expr-type x)))))
(define psd-make-boolean
  (lambda (start end val)
    (psd-make-expr 'boolean start end val)))
(define psd-boolean?
  (let ((eq? eq?))
    (lambda (x) (eq? 'boolean (psd-expr-type x)))))
(define psd-make-string
  (lambda (start end val)
    (psd-make-expr 'string start end val)))
(define psd-string?
  (let ((eq? eq?))
    (lambda (x) (eq? 'string (psd-expr-type x)))))
(define psd-make-char
  (lambda (start end val)
    (psd-make-expr 'char start end val)))
(define psd-char?
  (let ((eq? eq?))
    (lambda (x) (eq? 'char (psd-expr-type x)))))
(define psd-make-vector
  (lambda (start end contents)
    (psd-make-expr 'vector start end contents)))
(define psd-vector?
  (let ((eq? eq?))
    (lambda (x) (eq? 'vector (psd-expr-type x)))))
(define psd-vector-contents psd-expr-contents)

;;;
;;; psd-map is a map that accepts a pexp and returns a sexp.
;;; 


(define psd-map
  (let ((not not) (cons cons))
    (lambda (proc pexp)
      (cond ((psd-null? pexp)
	     '())
	    ((not (psd-pair? pexp))
	     (proc pexp))
	    (else
	     (cons (proc (psd-car pexp))
		   (psd-map proc (psd-cdr pexp))))))))

;;;
;;; pexp->sexp converts a pexp into a sexp
;;; 

(define pexp->sexp
  (let ((cons cons) (eof-object? eof-object?))
    (lambda (pexp)
      (cond ((eof-object? pexp) pexp)
	    ((psd-null? pexp) '())
	    ((psd-pair? pexp)
	     (cons (pexp->sexp (psd-car pexp))
		   (pexp->sexp (psd-cdr pexp))))
	    ((psd-vector? pexp)
	     (apply vector (map pexp->sexp (psd-vector-contents pexp))))
	    (else
	     (psd-expr-contents pexp))))))
