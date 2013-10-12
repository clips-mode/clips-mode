;;; inf-clips.el --- Inferior Clips mode.

;;;************************************************************************
;;; Basado en inf-jess.el de:

;; Copyright (C) 1999 by David E. Young.

;; Author: David E. Young <david.young@fnc.fujitsu.com>
;; Keywords: languages, clips

;; Version 0.5 of 9 August 1999.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;; 02111-1307  USA
;;;************************************************************************

;;; Code:

(require 'comint)
(require 'clips-mode)

(defvar inferior-clips-mode-map nil)

(defvar inferior-clips-mode-hook nil
  "*Hooks for customising Clips mode.")

(defvar inferior-clips-load-hook nil
  "*Hooks run after this module is loaded.")

(defvar inferior-clips-buffer nil
  "The current inferior Clips process buffer.")

(defvar inferior-clips-program nil
  "*Defines a program name or function used to construct an inferior
Clips process.
If this variable evaluates to a string, it is interpreted as a
'self-contained' executable (eg. shell script) that requires no
arguments. If this variable's value is a function, it should evaluate
to a list of arguments which are handed to the Java virtual machine as
defined by `inferior-clips-vm'.")

(defvar inferior-clips-vm "java"
  "*Defines the virtual machine used to run an inferior Clips process.")

(defvar source-modes '(inferior-clips-mode)
  "*Used to determine whether or not a buffer contains Clips source code.")

(defvar previous-dir/file nil
  "Records the last directory and file used in loading. Holds a dotted
pair of the form `(DIRECTORY . FILE)' describing the last
`load-file' command.")

(when (not inferior-clips-mode-map)
  (setq inferior-clips-mode-map
    (copy-keymap comint-mode-map)))
(define-key clips-mode-map "\C-c\C-l" 'inf-clips-load-file)
(define-key inferior-clips-mode-map "\C-c\C-l" 'inf-clips-load-file)
(define-key inferior-clips-mode-map "\C-ci" 'inf-clips-reset-engine) ; initialize engine
(define-key inferior-clips-mode-map "\C-cl" 'inf-clips-load-file)
(define-key inferior-clips-mode-map "\C-cg" 'inf-clips-run-engine) ; "go"...
(define-key inferior-clips-mode-map "\C-cf" 'inf-clips-get-facts)
(define-key inferior-clips-mode-map "\C-cr" 'inf-clips-get-rules)

;; These keys augment 'clips-mode-map' with behavior specific to an
;; inferior Clips process...

(define-key clips-mode-map "\C-x\C-e" 'inf-clips-eval-last-sexp)  ; GNU convention
(define-key clips-mode-map "\M-\C-x" 'inf-clips-eval-deffunction) ; GNU convention
(define-key clips-mode-map "\C-ce" 'inf-clips-eval-region)
(define-key clips-mode-map "\C-ct" 'inf-clips-eval-deftemplate)

(defun inferior-clips-mode ()
  "Major mode for interacting with an inferior Clips process.
Runs a Clips interpreter as a subprocess of Emacs, with Clips I/O
through an Emacs buffer.  Variable `inferior-clips-program'
controls how the Clips interpreter is run.

For information on running multiple processes in multiple buffers, see
documentation for variable `inferior-clips-buffer'.

\\{inferior-clips-mode-map}

Customisation: Entry to this mode runs the hooks on `comint-mode-hook' and
`inferior-clips-mode-hook' (in that order).

You can send text to the inferior Clips process from other buffers containing
Clips source.
    switch-to-inferior-clips switches the current buffer to the Clips process buffer.
    clips-eval-region sends the current region to the Clips process.

    Prefixing the clips-eval-region command with a
    \\[universal-argument] causes a switch to the Clips process buffer
    after sending the text.

Commands:
Return after the end of the process' output sends the text from the
    end of process to point.
Return before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
Delete converts tabs to spaces as it moves back.
Tab indents for Clips; with argument, shifts rest
    of expression rigidly with the current line.
C-M-q does Tab on each line starting within following expression.
Paragraphs are separated only by blank lines.  Semicolons start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it."
  (interactive)
  (kill-all-local-variables)
  (comint-mode)
  (setq major-mode 'inferior-clips-mode)
  (setq mode-name "Inferior Clips")
  (setq mode-line-process '(": %s"))
  (use-local-map inferior-clips-mode-map)
  (setq comint-input-sentinel 'ignore)
  (run-hooks 'inferior-clips-mode-hook))

;;;###autoload
(defun run-clips (&optional image)
  "Run an inferior Clips process, with input and output via buffer
`*clips*'. If there is a process already running in `*clips*', just
switch to that buffer.
With argument, allows you to edit the command line (default is value
of `inferior-clips-program').  Runs the hooks from
`inferior-clips-mode-hook' (after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (interactive (list
                (and current-prefix-arg
                     (read-string "Run Clips like this: "))))
  (when (not (comint-check-proc "*clips*"))
    (let* ((image
            (or image inferior-clips-program))
           (buffer
            (cond ((stringp image)
                   (make-comint "clips" image))
                  ((functionp image)
                   (apply 'make-comint
                          "clips" inferior-clips-vm nil
                          (funcall image)))
                  (t
                   (error "Variable `inferior-clips-program' must be either stringp or
functionp")))))
      (set-buffer buffer)
      (inferior-clips-mode)))
  (setq inferior-clips-buffer "*clips*")
  (switch-to-buffer inferior-clips-buffer))

(defun test-run-clips ()
  (interactive)
  (setq inferior-clips-program
        #'(lambda ()
            '("-classpath"
              "/files/devel/jem/classes/clips.jar"
              "clips.Main")))
  (run-clips))

(defun inf-clips-send-request(req)
  (let ((proc (inferior-clips-process)))
    (comint-send-string proc (concat req "\n"))))

(defun inf-clips-load-file (fname)
  "Load a Clips source file into the inferior Clips process."
  (interactive
   (comint-get-source "Load Clips file: "
                      previous-dir/file
                      source-modes t))
  (comint-check-source fname)
  (setq previous-dir/file
	(cons (file-name-directory fname)
	      (file-name-nondirectory fname)))
  (inf-clips-send-request (format "(load \"%s\")" fname))
  (switch-to-inferior-clips t))

(defun inf-clips-get-facts ()
  "Retrieve the fact list from the inferior Clips process."
  (interactive)
  (inf-clips-send-request "(facts)"))

(defun inf-clips-get-rules ()
  "Retrieve the rule list from the inferior Clips process."
  (interactive)
  (inf-clips-send-request "(rules)"))

(defun inf-clips-reset-engine ()
  "Reset the inference engine running in the inferior Clips process."
  (interactive)
  (inf-clips-send-request "(reset)"))

(defun inf-clips-run-engine ()
  "Run the inference engine in the inferior Clips process."
  (interactive)
  (inf-clips-send-request "(run)"))

(defun inferior-clips-process ()
  (let ((proc (get-buffer-process
               (if (eq major-mode 'inferior-clips-mode)
                   (current-buffer)
                 inferior-clips-buffer))))
    (or proc
        (error "No Clips sub-process; see variable `inferior-clips-buffer'"))))

(defun switch-to-inferior-clips (eob-p)
  "Switch to the inferior Clips process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (if (get-buffer-process inferior-clips-buffer)
      (let ((pop-up-frames
            ;; Be willing to use another frame
            ;; that already has the window in it.
             (or pop-up-frames
                 (get-buffer-window inferior-clips-buffer t))))
        (pop-to-buffer inferior-clips-buffer))
    (run-clips inferior-clips-program))
  (when eob-p
    (push-mark)
    (goto-char (point-max))))

(defun inf-clips-eval-region (start end &optional and-go)
  "Send the current region to the inferior Clips process.
Prefix argument forces switch to Clips buffer afterwards."
  (interactive "r\nP")
  (let ((proc (inferior-clips-process)))
    (comint-send-region proc start end)
    (comint-send-string proc "\n")
    (if and-go
        (switch-to-inferior-clips t))))

(defun inf-clips-eval-last-sexp (&optional and-go)
  "Send the previous sexp to the inferior Clips process.
Prefix argument means switch to the Clips buffer afterwards."
  (interactive "P")
  (inf-clips-eval-region
   (save-excursion
     (backward-sexp) (point))
   (point) and-go))

(defun inf-clips-eval-form (&optional and-go)
  "Send the current form to the inferior Clips process.
Prefix argument means switch to the Clips buffer afterwards."
  (interactive "P")
  (save-excursion
    (end-of-defun)
    (skip-chars-backward " \t\n\r\f") ;  Makes allegro happy
    (let ((end (point)))
      (beginning-of-defun)
      (inf-clips-eval-region (point) end)))
  (if and-go
      (switch-to-inferior-clips t)))

(defun inf-clips-eval-deffunction (&optional and-go)
  "Send the current deffunction to the inferior Clips process.
Prefix argument means switch to the Clips buffer afterwards."
  (interactive "P")
  (inf-clips-eval-form and-go))

(defun inf-clips-eval-defrule (&optional and-go)
  "Send the current defrule to the inferior Clips process.
Prefix argument means switch to the Clips buffer afterwards."
  (interactive "P")
  (inf-clips-eval-form and-go))

(defun inf-clips-eval-deftemplate (&optional and-go)
  "Send the current deftemplate to the inferior Clips process.
Prefix argument means switch to the Clips buffer afterwards."
  (interactive "P")
  (inf-clips-eval-form and-go))

(run-hooks 'inferior-clips-load-hook)

(provide 'inf-clips)

;;; inf-clips.el ends here
