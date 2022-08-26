;;;;
;;;; $Id$
;;;;
;;;; psd -- a portable Scheme debugger, version 1.1
;;;; Copyright (C) 1992 Pertti Kellomaki, pk@cs.tut.fi

;;;; 
;;;; Written by Pertti Kellomaki, pk@cs.tut.fi
;;;;

;;;;
;;;; $Log: version.scm,v $
;;;;

(define *psd-version* "1.1")

(define (psd-announce-version)
  (display "psd version ")
  (display *psd-version*)
  (newline))
