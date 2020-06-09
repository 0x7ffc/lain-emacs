;; lain-emacs-lisp.el -*- lexical-binding: t; -*-

(defvar lain-elisp-fns
  '(smartparens-strict-mode
    evil-cleverparens-mode
    highlight-parentheses-mode
    paren-face-mode
    indent-guide-mode)
  "Functions to run for all elisp modes")

(use-package elisp-mode
  :ensure nil
  :gfhook
  ('(emacs-lisp-mode-hook lisp-interaction-mode) lain-elisp-fns)
  :config
  (lain/set-major-mode-leader-keys emacs-lisp-mode
    "c" '(:ignore t :wk "compile")
    "e" '(:ignore t :wk "eval")
    "d" '(:ignore t :wk "debug")
    "cc" 'emacs-lisp-byte-compile
    "ee" 'lain/eval-current-form-sp
    "es" 'lain/eval-current-symbol-sp
    "ef" 'lain/eval-current-form
    "eF" 'eval-defun
    "eb" 'eval-buffer)
  ;; Borrowed from Spacemacs
  (defun lain/eval-current-form ()
    "Find and evaluate the current def* or set* command.
Unlike `eval-defun', this does not go to topmost function."
    (interactive)
    (save-excursion
      (search-backward-regexp "(def\\|(set")
      (forward-list)
      (call-interactively 'eval-last-sexp)))
  (defun lain/eval-current-form-sp (&optional arg)
    "Call `eval-last-sexp' after moving out of `arg' levels of parentheses"
    (interactive "p")
    (let ((evil-move-beyond-eol t))
      (save-excursion
	(sp-up-sexp arg)
	(call-interactively 'eval-last-sexp))))
  (defun lain/eval-current-symbol-sp ()
    "Call `eval-last-sexp' on the symbol around point. "
    (interactive)
    (let ((evil-move-beyond-eol t))
      (save-excursion
	(sp-forward-symbol)
	(call-interactively 'eval-last-sexp)))))

(use-package ielm
  :gfhook 'turn-on-smartparens-strict-mode
  :general
  (lain-emacs-lisp-mode-map
   "'" 'ielm)
  ((normal insert)
   inferior-emacs-lisp-mode-map
   "C-t" 'comint-next-input
   "C-n" 'comint-previous-input
   "C-r" 'comint-history-isearch-backward-regexp))

(use-package macrostep
  :gfhook 'evil-normalize-keymaps
  :general
  (normal
   macrostep-keymap
   "q"   'macrostep-collapse-all
   "e"   'macrostep-expand
   "u"   'macrostep-collapse
   "C-t" 'macrostep-next-macro
   "C-n" 'macrostep-prev-macro)
  (lain-emacs-lisp-mode-map
   "em" 'macrostep-expand))

(use-package edebug
  :gfhook 'evil-normalize-keymaps
  :general
  (lain-emacs-lisp-mode-map
   "dd" 'lain/edebug-defun
   "dD" 'edebug-defun)
  (edebug-mode-map
   "t"   'evil-next-line
   "T"   'edebug-trace-mode
   "C-t" 'edebug-Trace-fast-mode
   "h"   'evil-backward-char
   "H"   'edebug-goto-here
   "n"   'evil-previous-line
   "N"   'edebug-next-mode)
  :config
  (evil-set-initial-state edebug-mode 'normal)
  (defun lain/edebug-defun ()
    "Find and edebug the current def* command.
Unlike `edebug-defun', this does not go to topmost function."
    (interactive)
    (save-excursion
      (search-backward-regexp "(def")
      (mark-sexp)
      (narrow-to-region (point) (mark))
      (deactivate-mark)
      (edebug-defun)
      (message "Edebug enabled")
      (widen))))

(use-package aggressive-indent
  :diminish
  :ghook 'emacs-lisp-mode-hook)

(use-package elisp-def
  :diminish
  :ghook 'emacs-lisp-mode-hook
  :general
  ((normal insert)
   elisp-def-mode-map
   "M-." 'elisp-def))

(provide 'lain-emacs-lisp)
