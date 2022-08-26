;;;;
;;;; $Id: psd.el,v 1.13 1993/10/07 08:27:46 pk Exp $
;;;;
;;;
;;; Modified from gdb.el by Pertti Kellom\"aki, pk@cs.tut.fi
;;;

;;; Run psd under GNU Emacs 18.58
;;; Copyright (C) 1992 Pertti Kellom\"aki.

;;; Author: Pertti Kellom\"aki, Tampere University of Technology, Finland
;;;    pk@cs.tut.fi

;;; Description of psd interface:

;;; A facility is provided for the simultaneous display of the source code
;;; in one window, while using psd to step through a function in the
;;; other.  A small arrow in the source window, indicates the current
;;; line.

;;; Starting up:

;;; In order to use this facility, start up an inferior scheme
;;; interpreter. Then give the command "M-x psd-mode". This will load
;;; the psd system into your interpreter. The command "M-x
;;; psd-debug-file", usually bound to "C-c d" will instrument and load a
;;; Scheme file into the interpreter. The procedures in the file are
;;; instrumented so that executing them invokes the debugger. See the
;;; manual for further details. 

;;; psd-display-frame is the basic display function.  It tries to display
;;; in the other window, the file and line corresponding to the current
;;; position in the Scheme window.  For example after a psd-step, it would
;;; display the line corresponding to the position for the last step.

;;; psd-display-frame is invoked automatically when a filename-and-line-number
;;; appears in the output.

;;;
;;; $Log: psd.el,v $
; Revision 1.13  1993/10/07  08:27:46  pk
; Emacs lisp does not have keyword else... replaced one with t.
; Schemism!
;
; Revision 1.12  1993/10/06  13:57:54  pk
; Do not delete the temporary files after instrumentation , because we
; would have to wait until instrumentation is complete before deleting
; the file. This would mean freezing the whole Emacs session. Rather let
; a few files stay in /tmp. We need to delete the file holding the
; instrumented version, because some Schemes do not like write on top of
; an existing file.
;
; Revision 1.11  1993/10/06  12:55:33  pk
; (psd-mode): Moved running psd-mode-hook to the correct place. It is now
; run only when turning on psd-mode.
;
; (psd-mode): Fixed selecting the correct initialization file. Used to
; try to load file "".
;
; Revision 1.10  1993/10/06  10:39:32  pk
; Added psd-mode-hook.
;
; Revision 1.9  1993/10/06  09:01:43  pk
; (psd-debug-file): delete the temporary file before instrumentation, if
; one exists, and always delete it after instrumentation.
;
; (psd-send-definition): delete the temporary file after
; instrumentation.
;
; Revision 1.8  1993/10/04  14:04:20  pk
; Added a new variable psd-using-slib. If it is set to true, psd-mode
; will look for a file psd-slib.scm. If set to nil, psd-mode will first
; try to load the implementation dependent initializiation (psd-foo.scm
; for implementation foo), and then resort to the generic psd.scm.
;
; Revision 1.7  1993/09/27  10:52:15  pk
; Added psd-break and binding of C-c b to psd-break in both scheme-mode
; and inferior-scheme-mode.
;
; Revision 1.6  1993/09/23  06:49:56  pk
; Moved definition of the Scheme variable psd-directory from the psd*.scm
; files to psd.el, which sends it to the Scheme process. This way, the path
; to psd needs to be specified only once.
;
; Revision 1.5  1993/09/22  12:25:34  pk
; (psd-debug-file): Accepts an optional argument so that the user does
; not necessarily end up in the Scheme buffer.
;
; (psd-send-definition): Pass an extra argument to psd-debug-file
; telling we do not want to end up in the Scheme buffer.
;
; Added psd-clear-breakpoint.
;
; Added fancy menus for Emacs 19.
;
; Revision 1.4  1993/09/06  15:00:37  pk
; Initialization of psd and instrumentation commands now return a
; meaningful value instead of the value that load happens to return.
;
; The temporary buffer used for instrumenting a single definition is
; killed when it is no longer needed.
;
; Revision 1.3  1993/09/02  06:56:24  pk
; Added Id to header, now provides Emacs feature 'psd
;
; Revision 1.2  1993/07/30  09:49:07  pk
; Typo fixes.
;
; Revision 1.1  1993/07/30  09:45:38  pk
; Initial revision
;
;;; 
;;; SCCS log:
;;; D 1.9	92/07/08 11:50:46 pk	9 8	00009/00005/00378
;;; M-x psd-mode now understands prefix arg.
;;; D 1.8	92/07/07 14:36:44 pk	8 7	00003/00003/00380
;;; Fixed the key bindings of Scheme mode.
;;; D 1.7	92/07/07 14:31:17 pk	7 6	00005/00000/00378
;;; Documented scheme-or-psd-send--definition-*.
;;; D 1.6	92/07/07 13:41:51 pk	6 5	00012/00005/00366
;;; Changed psd-file-name to psd-directory.
;;; Now looks for psd-foo.scm for scheme implementation foo.
;;; D 1.5	92/06/30 10:09:18 pk	5 4	00001/00001/00370
;;; Changed psd file name to /usr/local/lib/psd/psd.scm.
;;; D 1.4	92/06/30 10:07:26 pk	4 3	00001/00001/00370
;;; Psd-reset now sends a newline after (psd-reset).
;;; D 1.3	92/06/26 16:51:13 pk	3 2	00116/00018/00255
;;; Added commands to Scheme mode, breakpoints work.
;;; D 1.2	92/06/23 12:30:16 pk	2 1	00101/00265/00172
;;; No major changes.
;;; D 1.1	92/05/27 10:32:31 pk	1 0	00437/00000/00000
;;; date and time created 92/05/27 10:32:31 by pk
;;; 

;; Where psd resides. Edit this to suit your installation.  Not needed
;; for SLIB installation.
(defvar psd-directory "/home/kaarne-b/pk/psd/"
  "Path of the directory that contains psd.")

(defvar psd-using-slib t
  "*If you are using slib set this to true. All
the differences between implementations should be handled by
slib. If this is set to true, psd will load the file
psd-slib.scm. If it is set to nil, psd will try first to find a file
psd-foo.scm, where foo is the name of your Scheme interpreter, and
if that fails, it will load the file psd.scm.")

(defvar psd-mode-hook '()
  "Hook to run upon entering psd-mode")

(require 'cmuscheme)
(provide 'psd)

;; Add psd into the minor modes.
(defvar psd-mode nil "Indicator for psd-mode")

(or (assq 'psd-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(psd-mode " Psd") minor-mode-alist)))

;; The temporary files that are used for sending stuff to psd.
(defvar *psd-tmp-source-file* (make-temp-name "/tmp/psd1"))
(defvar *psd-tmp-target-file* (make-temp-name "/tmp/psd2"))

(defun psd-mode (&optional arg)
  "Toggle psd-mode, with argument turn on psd-mode.

Psd-mode is a minor mode for interacting with a psd running in an
inferior Scheme buffer. Psd is a Scheme debugger that debugs the
program by instrumenting it.

The command `psd-debug-file', which is bound to \\[psd-debug-file]
prepares a Scheme file for debugging and loads it into the Scheme 
interpreter.

The Scheme mode commands `C-c e', `C-c c-e' and `ESC C-x' now run the
command `psd-send-definition' or `psd-send-definition-and-go' if given
an argument. `C-c d' prepares a Scheme file for debugging and loads it.

For instance to debug a procedure, go to its definition
and type `C-u C-c C-e'. This will put you into the inferior Scheme
buffer ready to try out the definition.

The command `psd-set-breakpoint' or `C-x SPC' sets a breakpoint in
current line when given in a Scheme buffer.

Entering psd-mode also loads psd into the Scheme interpreter.

If the debugger does not seem to work properly, try the command ``M-x
psd-reset'', which will clear breakpoints and restore the debugger
into its initial state."
  (interactive "p")
  (make-local-variable 'psd-mode)
  (if (and (<= arg 1)
	   psd-mode)

      ;; turn off psd-mode
      (progn 
	(setq psd-filter-accumulator nil)
	(setq psd-last-frame nil)
	(set-process-filter (get-buffer-process (current-buffer))
			    nil)
	(set-process-sentinel (get-buffer-process (current-buffer))
			      nil)
	(setq psd-mode nil))

    ;; otherwise set up psd-mode
    (setq psd-mode t)
    (make-local-variable 'psd-filter-accumulator)
    (setq psd-filter-accumulator nil)
    (make-local-variable 'psd-last-frame)
    (setq psd-last-frame nil)
    (local-set-key "\C-cd" 'psd-debug-file)
    (define-key scheme-mode-map "\M-\C-x" 'scheme-or-psd-send-definition);gnu convention
    (define-key scheme-mode-map "\C-ce"    'scheme-or-psd-send-definition)
    (define-key scheme-mode-map "\C-c\C-e" 'scheme-or-psd-send-definition-and-go)
    (define-key scheme-mode-map "\C-cd" 'psd-debug-file)
    (define-key scheme-mode-map "\C-cb" 'psd-break)
    (define-key inferior-scheme-mode-map "\C-cb" 'psd-break)
    (define-key scheme-mode-map "\C-x " 'psd-set-breakpoint)
    (set-process-filter (get-buffer-process (current-buffer))
			'psd-filter)
    (set-process-sentinel (get-buffer-process (current-buffer))
			  'psd-sentinel)
    (send-string
     "scheme"
     (if  psd-using-slib
	 "(require 'portable-scheme-debugger)"
;;;	 "(load (in-vicinity (sub-vicinity (library-vicinity) \"psd\")
;;;			    \"psd-slib\"
;;;			    (scheme-file-suffix)))"
	 (concat "(begin "
		 "(define psd-directory \""
		 psd-directory
			 
		 "\") (load \""
		 (let ((implementation-file
			(concat psd-directory "psd-"
				scheme-program-name
				".scm"))
		       (default-file
			 (concat psd-directory "psd.scm")))
		   (cond ((file-exists-p implementation-file)
			  implementation-file)
			 (t default-file)))
		 "\"\) 'psd-mode-initialized)\n")))
    (run-hooks 'psd-mode-hook)))


;; This function is responsible for inserting output from Scheme
;; into the buffer.
;; Aside from inserting the text, it notices and deletes
;; each filename-and-line-number;
;; that psd prints to identify the selected frame.
;; It records the filename and line number, and maybe displays that file.

(defun psd-filter (proc string)
  (let ((inhibit-quit t))
    (if psd-filter-accumulator
	(psd-filter-accumulate-marker proc
				      (concat psd-filter-accumulator string))
      (psd-filter-scan-input proc string))))

(defun psd-filter-accumulate-marker (proc string)
  (setq psd-filter-accumulator nil)
  (if (> (length string) 1)
      (if (= (aref string 1) ?\032)
	  (let ((end (string-match "\n" string)))
	    (if end
		(progn
		  (let* ((first-colon (string-match ":" string 2))
			 (second-colon
			  (string-match ":" string (1+ first-colon))))
		    (setq psd-last-frame
			  (cons (substring string 2 first-colon)
				(string-to-int
				 (substring string (1+ first-colon)
					    second-colon)))))
		  (setq psd-last-frame-displayed-p nil)
		  (psd-filter-scan-input proc
					 (substring string (1+ end))))
	      (setq psd-filter-accumulator string)))
	(psd-filter-insert proc "\032")
	(psd-filter-scan-input proc (substring string 1)))
    (setq psd-filter-accumulator string)))

(defun psd-filter-scan-input (proc string)
  (if (equal string "")
      (setq psd-filter-accumulator nil)
    (let ((start (string-match "\032" string)))
      (if start
	  (progn (psd-filter-insert proc (substring string 0 start))
		 (psd-filter-accumulate-marker proc
					       (substring string start)))
	(psd-filter-insert proc string)))))

(defun psd-filter-insert (proc string)
  (let ((moving (= (point) (process-mark proc)))
	(output-after-point (< (point) (process-mark proc)))
	(old-buffer (current-buffer))
	start)
    (set-buffer (process-buffer proc))
    (unwind-protect
	(save-excursion
	  ;; Insert the text, moving the process-marker.
	  (goto-char (process-mark proc))
	  (setq start (point))
	  (insert string)
	  (set-marker (process-mark proc) (point))
					;(psd-maybe-delete-prompt)
	  ;; Check for a filename-and-line number.
	  (psd-display-frame
	   ;; Don't display the specified file
	   ;; unless (1) point is at or after the position where output appears
	   ;; and (2) this buffer is on the screen.
	   (or output-after-point
	       (not (get-buffer-window (current-buffer))))
	   ;; Display a file only when a new filename-and-line-number appears.
	   t))
      (set-buffer old-buffer))
    (if moving (goto-char (process-mark proc)))))

(defun psd-sentinel (proc msg)
  (cond ((null (buffer-name (process-buffer proc)))
	 ;; buffer killed
	 ;; Stop displaying an arrow in a source file.
	 (setq overlay-arrow-position nil)
	 (set-process-buffer proc nil))
	((memq (process-status proc) '(signal exit))
	 ;; Stop displaying an arrow in a source file.
	 (setq overlay-arrow-position nil)
	 ;; Fix the mode line.
	 (setq mode-line-process
	       (concat ": "
		       (symbol-name (process-status proc))))
	 (let* ((obuf (current-buffer)))
	   ;; save-excursion isn't the right thing if
	   ;;  process-buffer is current-buffer
	   (unwind-protect
	       (progn
		 ;; Write something in *compilation* and hack its mode line,
		 (set-buffer (process-buffer proc))
		 ;; Force mode line redisplay soon
		 (set-buffer-modified-p (buffer-modified-p))
		 (if (eobp)
		     (insert ?\n mode-name " " msg)
		   (save-excursion
		     (goto-char (point-max))
		     (insert ?\n mode-name " " msg)))
		 ;; If buffer and mode line will show that the process
		 ;; is dead, we can delete it now.  Otherwise it
		 ;; will stay around until M-x list-processes.
		 (delete-process proc))
	     ;; Restore old buffer, but don't restore old point
	     ;; if obuf is the psd buffer.
	     (set-buffer obuf))))))

(defun psd-display-frame (&optional nodisplay noauto)
  "Find, obey and delete the last filename-and-line marker from PSD.
The marker looks like \\032\\032FILENAME:LINE:CHARPOS\\n.
Obeying it means displaying in another window the specified file and line."
  (interactive)
  (and psd-last-frame (not nodisplay)
       (or (not psd-last-frame-displayed-p) (not noauto))
       (progn (psd-display-line (car psd-last-frame) (cdr psd-last-frame))
	      (setq psd-last-frame-displayed-p t))))

;; Make sure the file named TRUE-FILE is in a buffer that appears on the screen
;; and that its line LINE is visible.
;; Put the overlay-arrow on the line LINE in that buffer.

(defun psd-display-line (true-file line)
  (let* ((buffer (find-file-noselect true-file))
	 (window (display-buffer buffer t))
	 (pos))
    (save-excursion
      (set-buffer buffer)
      (save-restriction
	(widen)
	(goto-line line)
	(setq pos (point))
	(setq overlay-arrow-string "=>")
	(or overlay-arrow-position
	    (setq overlay-arrow-position (make-marker)))
	(set-marker overlay-arrow-position (point) (current-buffer)))
      (cond ((or (< pos (point-min)) (> pos (point-max)))
	     (widen)
	     (goto-char pos))))
    (set-window-point window overlay-arrow-position)))

;;
;; Instrument a Scheme file and load it into Scheme.
;;

(defun psd-debug-file (file-name &optional no-switch)
  "Instrument a Scheme file and load it into the Scheme interpreter."
  (interactive (comint-get-source "Debug Scheme file: "
				  scheme-prev-l/c-dir/file
				  scheme-source-modes t)) ; T because LOAD 
					; needs an exact name
  (comint-check-source file-name)	; Check to see if buffer needs saved.
  (setq scheme-prev-l/c-dir/file (cons (file-name-directory    file-name)
				       (file-name-nondirectory file-name)))
  (if (file-exists-p *psd-tmp-target-file*)
      (delete-file *psd-tmp-target-file*))
  (send-string "scheme" (concat "(begin (psd-instrument-file \""
				file-name
				"\" \""
				*psd-tmp-target-file*
				"\") (load \""
				*psd-tmp-target-file*
				"\"\) 'ok)\n"))
  (if no-switch
      nil
    (switch-to-scheme t)))

;;;
;;; Write a Scheme definition into a file, instrument it with psd and
;;; load it into the interpreter. Use #line directives for informing
;;; psd where the definition originally came from.
;;; 

(defun psd-send-definition ()
  "Instrument a definition and load it into Scheme."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (let ((beginning (point))
	  (line (1+ (count-lines 1 (point))))
	  (char (1+ (current-column)))
	  (file (buffer-file-name)))
      (end-of-defun)
      (copy-region-as-kill beginning (point))
      (find-file *psd-tmp-source-file*)
      (erase-buffer)
      (insert "#line \"" file "\" "
	      (int-to-string line) " "
	      (int-to-string char) " #\n")
      (yank)
      (save-buffer 0)
      (kill-buffer (current-buffer))
      (psd-debug-file *psd-tmp-source-file* t))))

(defun psd-send-definition-and-go ()
  "Instrument a definition and load it into Scheme. Switches to the
Scheme buffer."
  (interactive)
 (psd-send-definition)
 (switch-to-scheme t))

;;;
;;; These are installed into scheme-mode-map in place of the normal commands
;;;

(defun scheme-or-psd-send-definition (&optional arg)
  "Without argument, send a definition to the Scheme process.
With argument, instrument a definition and send it to the Scheme process."
  (interactive "P")
  (if arg
      (psd-send-definition)
    (scheme-send-definition)))
  
(defun scheme-or-psd-send-definition-and-go (&optional arg)
  "Without argument, send a definition to the Scheme process.
With argument, instrument a definition and send it to the Scheme process.
Switches to the Scheme buffer."
  (interactive "P")
  (if arg
      (psd-send-definition-and-go)
    (scheme-send-definition-and-go)))
  

;;;
;;; Set a breakpoint in current line. This command is ment to be used
;;; in buffers containing Scheme source code.
;;;

(defun psd-set-breakpoint ()
  "Set a breakpoint in current line. This command is ment to be used
in buffers containing Scheme source code." 
  (interactive)
  (send-string "scheme" (concat "(psd-set-breakpoint \""
				(buffer-file-name (current-buffer))
				"\" "
				(save-restriction
				  (save-excursion
				    (widen)
				    (beginning-of-line)
				    (1+ (count-lines 1 (point)))))
				")\n")))
;;;
;;; Clear a breakpoint in current line. This command is ment to be used
;;; in buffers containing Scheme source code.
;;;

(defun psd-clear-breakpoint ()
  "Clear a breakpoint in current line. This command is ment to be used
in buffers containing Scheme source code." 
  (interactive)
  (send-string "scheme" (concat "(psd-clear-breakpoint \""
				(buffer-file-name (current-buffer))
				"\" "
				(save-restriction
				  (save-excursion
				    (widen)
				    (beginning-of-line)
				    (1+ (count-lines 1 (point)))))
				")\n")))

;;;
;;; Enable break on entry to a procedure
;;;

(defun psd-break (name)
  "Enable break on entry to a named procedure."
  (interactive "sBreak on entry to procedure: ")
  (send-string "scheme" (concat "(psd-break '"
				name
				")\n")))

;;;
;;; Reset the psd runtime clearing all breakpoints and resetting the
;;; runtime system into the initial state.
;;;

(defun psd-reset ()
  "Reset the psd runtime clearing all breakpoints and resetting the
runtime system into the initial state."
  (interactive)
  (send-string "scheme" "(psd-reset)\n"))


;;;
;;; Menus for psd-mode (Emacs 19)
;;;

(if (not (string-equal (substring emacs-version 0 2) "19"))
    nil
  (define-key scheme-mode-map [menu-bar breakpoints]
    (cons "Breakpoints" (make-sparse-keymap "Breakpoints")))
  
  (define-key scheme-mode-map [menu-bar breakpoints psd-menu-clear-breakpoint]
    '("Clear breakpoint" . psd-clear-breakpoint))
  
  (define-key scheme-mode-map [menu-bar breakpoints psd-menu-set-breakpoint]
    '("Set breakpoint" . psd-set-breakpoint))

 )
