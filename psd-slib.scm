;;;;
;;;; $Id: psd-slib.scm,v 1.1 1993/10/04 14:05:23 pk Exp $
;;;;
;;;; psd -- a portable Scheme debugger, version 1.1
;;;; Copyright (C) 1992 Pertti Kellomaki, pk@cs.tut.fi

;;;;
;;;;  $Log: psd-slib.scm,v $
;;;; Revision 1.1  1993/10/04  14:05:23  pk
;;;; Initial revision
;;;;
;;;;  

;;;; 
;;;; Written by Pertti Kellomaki, pk@cs.tut.fi
;;;;
;;;; SLIB interface to load psd files.

;;; this is not portable.
(define psd:control-z (integer->char 26))

;;; If you are using slib, use the following.
(require 'debug)
(slib:load (in-vicinity (program-vicinity) "version"))
(slib:load (in-vicinity (program-vicinity) "instrum"))
(slib:load (in-vicinity (program-vicinity) "pexpr"))
(slib:load (in-vicinity (program-vicinity) "read"))
(slib:load (in-vicinity (program-vicinity) "runtime"))
(slib:load (in-vicinity (program-vicinity) "primitives"))

;;;
;;; Say hello
;;;

(psd-announce-version)
