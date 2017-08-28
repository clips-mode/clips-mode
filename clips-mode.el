;;; clips-mode.el --- Clips editing mode.

;; Copyright (C) 2013 Grant Rettke <grettke@acm.org>

;; Author: David E. Young <david.young@fnc.fujitsu.com>
;;         Andrey Kotlarski <m00naticus@gmail.com>
;;         Grant Rettke <grettke@acm.org>
;; Maintainer: Grant Rettke <grettke@acm.org>
;; Version: 0.7
;; Keywords: clips

;;;*****************************************************************************

;; Copyright © 1999 by David E. Young <david.young@fnc.fujitsu.com>.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation either version 2, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program if not, write to the Free Software Foundation, Inc., 59 Temple
;; Place, Suite 330, Boston, MA 02111-1307 USA.

;; Some parts of this program were borrowed from Franz Inc's Emacs Lisp
;; Interface (ELI) to Allegro Common Lisp. The following copyright fulfills my
;; obligation in that respect:

;; Copyright (c) 1987-1993 Franz Inc, Berkeley, Ca.

;; Permission is granted to any individual or institution to use, copy, modify,
;; and distribute this software, provided that this complete copyright and
;; permission notice is maintained, intact, in all copies and supporting
;; documentation.

;; Franz Incorporated provides this software "as is" without express or implied
;; warranty.

;;;*****************************************************************************

;; Copyright © 2010 by Andrey Kotlarski <m00naticus@gmail.com>.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation either version 2, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program if not, write to the Free Software Foundation, Inc., 59 Temple
;; Place, Suite 330, Boston, MA 02111-1307 USA.

;;;*****************************************************************************

;; Copyright © 2013 by Grant Rettke <grettke@acm.org>.

;; This file is part of clips-mode.

;; clips-mode is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; clips-mode is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;; A PARTICULAR PURPOSE. See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; clips-mode. If not, see <http://www.gnu.org/licenses/>.

;;;*****************************************************************************

;;; Commentary:

;; There are 5 steps necessary to utilize clips-mode:
;; 1. Install the package clips-mode preferrably via ELPA or Cask.
;; 2. Require it: (require 'clips-mode)
;; 3. Decide whether you want to use a natively compiled CLIPS executable or
;;    the CLIPSJNI version that runs on a Java Virtual Machine (JVM).
;; 4. If you are using a native version, then configure the program name
;;    like this: (setq inferior-clips-program "clips")
;; 5. If you are using the JVM version, then configure which JVM to use like
;;    this: (setq inferior-clips-vm "java") and then configure a function
;;    which returns a list of strings that are passed as arguments to the
;;    previously specified JVM that result in the CLIPJNI shell being executed.
;; 6. Read inf-clips.el to see the keybindings for executing CLIPS commands in
;;    the CLIPS shell.

;;; Code:

(require 'lisp-mode)

(defvar clips-mode-map nil)

(defvar clips-mode-hook nil
  "*Hooks for customising Clips mode.")

(defvar clips-mode-syntax-table nil
  "The value of which is the syntax table for Clips mode.")

(when (not clips-mode-map)
  (let ((map (make-sparse-keymap "Clips")))
    (setq clips-mode-map
          (nconc (make-sparse-keymap) (if (boundp 'shared-lisp-mode-map) shared-lisp-mode-map lisp-mode-shared-map )))
;;; 14-12-2009,Carmen: para que admita la que este definida, shared-lisp-mode-map o lisp-mode-shared-map
    (define-key clips-mode-map [menu-bar] (make-sparse-keymap))
    (define-key clips-mode-map [menu-bar clips]
      (cons "Clips" map))
    (define-key map [inf-clips-load-file]
      '("Load File" . inf-clips-load-file))
    (define-key map [run-clips] '("Run Inferior Clips" . run-clips))
    (define-key map [comment-region] '("Comment Region" . comment-region))
    (define-key map [indent-region] '("Indent Region" . indent-region))
    (define-key map [indent-line] '("Indent Line" . lisp-indent-line))))

(if (not clips-mode-syntax-table)
    (let ((i 0))
      (setq clips-mode-syntax-table (make-syntax-table))
      (while (< i ?0)
        (modify-syntax-entry i "_   " clips-mode-syntax-table)
        (setq i (1+ i)))
      (setq i (1+ ?9))
      (while (< i ?A)
        (modify-syntax-entry i "_   " clips-mode-syntax-table)
        (setq i (1+ i)))
      (setq i (1+ ?Z))
      (while (< i ?a)
        (modify-syntax-entry i "_   " clips-mode-syntax-table)
        (setq i (1+ i)))
      (setq i (1+ ?z))
      (while (< i 128)
        (modify-syntax-entry i "_   " clips-mode-syntax-table)
        (setq i (1+ i)))
      (modify-syntax-entry ?  "    " clips-mode-syntax-table)
      (modify-syntax-entry ?\t "    " clips-mode-syntax-table)
      (modify-syntax-entry ?\n ">   " clips-mode-syntax-table)
      (modify-syntax-entry ?\f ">   " clips-mode-syntax-table)
      (modify-syntax-entry ?\; "<   " clips-mode-syntax-table)
      (modify-syntax-entry ?` "'   " clips-mode-syntax-table)
      (modify-syntax-entry ?' "'   " clips-mode-syntax-table)
      (modify-syntax-entry ?, "'   " clips-mode-syntax-table)
      (modify-syntax-entry ?. "'   " clips-mode-syntax-table)
      (modify-syntax-entry ?# "'   " clips-mode-syntax-table)
      (modify-syntax-entry ?\" "\"    " clips-mode-syntax-table)
      (modify-syntax-entry ?\\ "\\   " clips-mode-syntax-table)
      (modify-syntax-entry ?\( "()  " clips-mode-syntax-table)
      (modify-syntax-entry ?\) ")(  " clips-mode-syntax-table)
      (modify-syntax-entry ?\[ "(]  " clips-mode-syntax-table)
      (modify-syntax-entry ?\] ")[  " clips-mode-syntax-table)
      (modify-syntax-entry ?*   "w   " clips-mode-syntax-table)
      ;; The next syntax entry doesn't work with these forms:
      ;;  `,.foo
      ;;  #.foo
      ;; but it works better with variables with .'s in them
      (modify-syntax-entry ?. "w   " clips-mode-syntax-table)
      (modify-syntax-entry ?\| "_   " clips-mode-syntax-table)
      (modify-syntax-entry ?\[ "_   " clips-mode-syntax-table)
      (modify-syntax-entry ?\] "_   " clips-mode-syntax-table)))

(defconst clips-font-lock-keywords-1
  (eval-when-compile
    (let ((clips-constructs
           (regexp-opt
            '("deffunction" "deftemplate" "defrule" "deffacts" "defgeneric"
              "defmodule" "defadvice" "defglobal" "defmethod"
              "definstance" "defclass")))
          (clips-identifier
           (let ((letter "a-zA-Z_$\-\300-\326\330-\366\370-\377")
                 (digit "0-9"))
             (concat "\\<\\([" letter "][" letter digit "]*\\)\\>"))))
      (list
       (cons (concat "\\<" clips-constructs "\\>\\s *" clips-identifier)
             `(,(+ 1 (regexp-opt-depth clips-constructs)) font-lock-function-name-face))
       (cons (concat "\\<\\(" clips-constructs "\\)\\>") 'font-lock-keyword-face))))
  "Subdued expressions to highlight in Clips modes.")

(defconst clips-font-lock-keywords-2
  (append clips-font-lock-keywords-1
          (eval-when-compile
            (let ((clips-builtins
                   (regexp-opt
                    '("slot" "multislot" "type" "default" "default-dynamic"
                      "extends" "crlf""range" "nil" "if" "then" "else" "while"
                      "progn" "progn$" "not" "or" "switch" "case" "and" "reset"
                      "assert" "test" "declare" "salience" "return" "bind"
                      "retract" "explicit" "unique" "node-index-hash" "halt"
                      "=>" "expand$" "delayed-do-for-all-facts" "do-for-fact"
                      "do-for-all-facts" "duplicate" "foreach"
		      "loop-for-count" "modify")))
                  (clips-connective-constraints
                   (regexp-opt '("|" "&"))))
              (list
               (cons (concat "\\_<\\(" clips-builtins "\\)\\_>") 'font-lock-builtin-face)
               (cons (concat "\\<\\(" clips-connective-constraints "\\)\\>")
                     'font-lock-builtin-face)))))
  "Gaudy expressions to highlight in Clips modes.")

(defvar clips-font-lock-keywords clips-font-lock-keywords-2
  "Default expressions to highlight in Clips modes.")

(defvar clips-imenu-generic-expression
  (list
   (list nil
         (purecopy
          (concat "^\\s-*("
                  (eval-when-compile
                    (regexp-opt '("deffunction" "defgeneric"
                                  "defadvice" "defmethod") t))
                  "\\s-+'?\\(\\(\\sw\\|\\s_\\)+\\)"))
         2)
   (list (purecopy "Variables/Instances")
         (purecopy
          (concat "^\\s-*("
                  (eval-when-compile
                    (regexp-opt '("defglobal" "definstance") t))
                  "\\s-+'?\\(\\(\\sw\\|\\s_\\)+\\)"))
         2)
   (list (purecopy "Types/Objects")
         (purecopy
          (concat "^\\s-*("
                  (eval-when-compile
                    (regexp-opt '("deftemplate" "defclass") t))
                  "\\s-+'?\\(\\(\\sw\\|\\s_\\)+\\)"))
         2)
   (list (purecopy "Rules/Facts")
         (purecopy (concat "^\\s-*("
                           (eval-when-compile
                             (regexp-opt '("defrule" "deffacts") t))
                           "\\s-+'?\\(\\(\\sw\\|\\s_\\)+\\)"))
         2))
  "Imenu generic expression for Clips mode.  See `imenu-generic-expression'.")

(defun clips-initialize-mode ()
  (set-syntax-table clips-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat page-delimiter "\\|$" ))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'fill-paragraph-function)
  (setq fill-paragraph-function 'lisp-fill-paragraph)
  ;; Adaptive fill mode gets in the way of auto-fill,
  ;; and should make no difference for explicit fill
  ;; because lisp-fill-paragraph should do the job.
  (make-local-variable 'adaptive-fill-mode)
  (setq adaptive-fill-mode nil)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'lisp-indent-line)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'outline-regexp)
  (setq outline-regexp ";;; \\|(....")
  (make-local-variable 'comment-start)
  (setq comment-start ";")
  (make-local-variable 'comment-start-skip)
  ;; Look within the line for a ; following an even number of backslashes
  ;; after either a non-backslash or the line beginning.
  (setq comment-start-skip "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+ *")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'lisp-comment-indent)
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression clips-imenu-generic-expression
        imenu-case-fold-search nil)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(clips-font-lock-keywords))
  (use-local-map clips-mode-map)
  (set-syntax-table clips-mode-syntax-table))

(defun clips-defrule-indent (indent-point state)
  (let* ((defrule-form-column (progn (goto-char (elt state 1))
                                     (current-column)))
         (this-form-end (progn (goto-char indent-point)
                               (forward-sexp 1)
                               (point)))
         (this-form (buffer-substring (progn (forward-sexp -1)
                                             (point))
                                      this-form-end)))
    (if (string-equal this-form "=>")
        (+ defrule-form-column 1)
      (+ defrule-form-column lisp-body-indent))))

(defun clips-if-indent (indent-point state)
  (let* ((if-form-column (progn (goto-char (elt state 1))
                                (current-column)))
         (this-form-end (progn (goto-char indent-point)
                               (forward-sexp 1)
                               (point)))
         (this-form (buffer-substring (progn (forward-sexp -1)
                                             (point))
                                      this-form-end))
         (last-form-column (progn (goto-char (elt state 2))
                                  (current-column)))
         (last-form (buffer-substring (point)
                                      (progn (forward-sexp 1)
                                             (point)))))
    (cond ((or (string-equal this-form "else")
               (string-equal this-form "then"))
           (+ if-form-column 1))
          ((or (string-equal last-form "else")
               (string-equal last-form "then"))
           (+ last-form-column lisp-body-indent))
          (t last-form-column))))

(put 'assert 'clips-indent-function 0)
(put 'bind 'clips-indent-function 1)
(put 'defrule 'clips-indent-function 'clips-defrule-indent)
(put 'delayed-do-for-all-facts 'clips-indent-function 2)
(put 'do-for-fact 'clips-indent-function 2)
(put 'do-for-all-facts 'clips-indent-function 2)
(put 'duplicate 'clips-indent-function 1)
(put 'foreach 'clips-indent-function 2)
(put 'if 'clips-indent-function 'clips-if-indent)
(put 'modify 'clips-indent-function 1)
(put 'progn 'clips-indent-function 0)
(put 'progn$ 'clips-indent-function 1)
(put 'switch 'clips-indent-function 1)
(put 'while 'clips-indent-function 1)

;; Mostly copy from `lisp-indent-function' in lisp-mode.el, with minor tweaks.
(defun clips-indent-function (indent-point state)
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
		(progn (goto-char calculate-lisp-indent-last-sexp)
		       (beginning-of-line)
		       (parse-partial-sexp (point)
					   calculate-lisp-indent-last-sexp 0 t)))
	    ;; Indent under the list or under the first sexp on the same
	    ;; line as calculate-lisp-indent-last-sexp.  Note that first
	    ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let ((function (buffer-substring (point)
					(progn (forward-sexp 1) (point))))
	    method)
	(setq method (or (function-get (intern-soft function)
                                       'clips-indent-function)
			 (get (intern-soft function) 'clips-indent-hook)))
	(cond ((or (eq method 'defun)
		   (and (null method)
			(> (length function) 3)
			(string-match "\\`def" function)))
	       (lisp-indent-defform state indent-point))
	      ((integerp method)
	       (lisp-indent-specform method state
				     indent-point normal-indent))
	      (method
		(funcall method indent-point state)))))))

;;;###autoload
(defun clips-mode ()
  "Major mode for editing Clips code.
Editing commands are similar to those of other Lisp-like modes.

In addition, if an inferior Clips process is running, some additional
commands will be defined for evaluating expressions and controlling
the interpreter. The status of the process will also be displayed in
the modeline of all Clips buffers.

Commands:
\\{clips-mode-map}
Entry to this mode calls the value of `clips-mode-hook' if that value
is non-nil."
  (interactive)
  (kill-all-local-variables)
  (clips-initialize-mode)
  (setq major-mode 'clips-mode)
  (setq mode-name "Clips")
  (setq-local lisp-indent-function 'clips-indent-function)
  (run-hooks 'clips-mode-hook))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.clp$" . clips-mode))

(provide 'clips-mode)

;;; clips-mode.el ends here
