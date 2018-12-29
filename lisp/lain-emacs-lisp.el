;; lain-emacs-lisp.el -*- lexical-binding: t; -*-

(use-feature lisp-mode
  :demand t
  :lain-major-mode emacs-lisp-mode
  :gfhook
  ('emacs-lisp-mode-hook 'turn-on-smartparens-strict-mode)
  ('emacs-lisp-mode-hook 'evil-cleverparens-mode)
  :general
  (lain-emacs-lisp-mode-map
   "c" 'emacs-lisp-byte-compile
   "e" 'lain/eval-current-form-sp
   "s" 'lain/eval-current-symbol-sp
   "f" 'lain/eval-current-form
   "d" 'edebug-defun
   "b" 'eval-buffer)
  :config
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
   "m" 'macrostep-expand))

(use-feature edebug
  :gfhook 'evil-normalize-keymaps
  :general
  (normal
   edebug-mode-map
   "S" 'edebug-stop
   "s" 'edebug-step-mode
   "n" 'edebug-next-mode
   "t" 'edebug-trace-mode
   "T" 'edebug-Trace-fast-mode
   "g" 'edebug-go-mode
   "c" 'edebug-continue-mode
   "C" 'edebug-Continue-fast-mode
   "G" 'edebug-Go-nonstop-mode
   "h" 'edebug-goto-here
   "f" 'edebug-forward-sexp
   "o" 'edebug-step-out
   "i" 'edebug-step-in
   "?" 'edebug-help
   "a" 'abort-recursive-edit
   "q" 'top-level
   "Q" 'edebug-eval-top-level-form
   "r" 'edebug-previous-result
   "d" 'edebug-backtrace
   "b" 'edebug-set-breakpoint
   "u" 'ebebug-unset-breadpoint
   "x" 'edebug-set-conditional-breakpoint
   "B" 'edebug-next-breakpoint
   "v" 'edebug-view-outside
   "p" 'edebug-bounce-point
   "w" 'edebug-where
   "W" 'edebug-toggle-save-windows
   "e" 'edebug-eval-expression))

(use-package indent-guide
  :diminish
  :ghook 'emacs-lisp-mode-hook)

(use-package aggressive-indent
  :diminish
  :ghook 'emacs-lisp-mode-hook)

;; (use-package lispyville
;;   :ghook 'emacs-lisp-mode-hook
;;   :config
;;   (lispyville-set-key-theme
;;    '((operators normal)
;;      c-w
;;      (prettify insert)
;;      (atom-movement normal visual)
;;      (slurp/barf-lispy)
;;      (wrap normal insert)
;;      additional
;;      additional-insert
;;      (additional-wrap normal insert)
;;      (escape insert))))

(provide 'lain-emacs-lisp)
