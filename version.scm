;;;;
;;;; $Id$
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

;;;;
;;;; $Log: version.scm,v $
;;;;

(define *psd-version* "1.1")

(define (psd-announce-version)
  (display "psd version ")
  (display *psd-version*)
  (newline))
