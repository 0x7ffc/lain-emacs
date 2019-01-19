;; lain-clojure.el -*- lexical-binding: t; -*-

(defvar lain-clojure-fns
  '(smartparens-strict-mode
    evil-cleverparens-mode
    highlight-parentheses-mode
    paren-face-mode
    subword-mode)
  "Functions to run for all clojure modes")

(defvar lain-clojure-major-modes
  '(clojure-mode
    clojurec-mode
    clojurescript-mode
    cider-repl-mode
    cider-clojure-interaction-mode)
  "All clojure related modes")

(defmacro lain/set-clojure-leader-keys (&rest body)
  (cons 'progn (--map `(let ((mode ',it))
			 (lain/set-major-mode-leader-keys ,it ,@body))
		      lain-clojure-major-modes)))
(put 'lain/set-clojure-leader-keys 'lisp-indent-function 'defun)

(use-package clojure-mode
  :gfhook lain-clojure-fns
  :config
  (lain/set-popup-rules
    '("^\\*cider-repl" :quit nil :select t)
    '("^\\*cider-repl-history" :side right :slot 2))
  (lain/set-clojure-leader-keys
    "s"  '(:ignore t :wk "repl")
    "sj" 'cider-jack-in
    "sJ" 'cider-jack-in-clojurescript
    "r" '(:ignore t :wk "refactor")
    "ri" 'clojure-cycle-if
    "rp" 'clojure-cycle-privacy
    "r#" 'clojure-convert-collection-to-set
    "r'" 'clojure-convert-collection-to-quoted-list
    "r(" 'clojure-convert-collection-to-list
    "r[" 'clojure-convert-collection-to-vector
    "r{" 'clojure-convert-collection-to-map
    "r:" 'clojure-toggle-keyword-string
    "rf" 'clojure-thread-first-all
    "rt" 'clojure-thread
    "rl" 'clojure-thread-last-all
    "rU" 'clojure-unwind-all
    "ru" 'clojure-unwind))

(use-package cider
  :gfhook
  'eldoc-mode
  ('(cider-repl-mode-hook cider-clojure-interaction-mode-hook)
   (cons 'eldoc-mode lain-clojure-fns))
  :general
  ((normal insert)
   cider-mode-map
   "M-." 'cider-find-var)
  ((normal insert)
   cider-repl-mode-map
   "C-t" 'cider-repl-next-input
   "C-n" 'cider-repl-previous-input)
  :config
  (setq
   nrepl-hide-special-buffers t
   nrepl-log-messages nil
   cider-font-lock-dynamically '(macro core function var)
   cider-overlays-use-font-lock t
   cider-prompt-for-symbol nil
   cider-repl-display-help-banner nil
   cider-repl-history-display-duplicates nil
   cider-repl-history-display-style 'one-line
   cider-repl-history-file
   (no-littering-expand-var-file-name "cider-repl-history")
   cider-repl-history-highlight-current-entry t
   cider-repl-history-quit-action 'delete-and-restore
   cider-repl-history-highlight-inserted-item t
   cider-repl-history-size 1000
   cider-repl-pop-to-buffer-on-connect 'display-only
   cider-repl-result-prefix ";; => "
   cider-repl-print-length 100
   cider-repl-use-clojure-font-lock t
   cider-repl-use-pretty-printing t
   cider-repl-wrap-history nil
   cider-stacktrace-default-filters '(tooling dup))
  (lain/set-clojure-leader-keys
    "d"  '(:ignore t :wk "debug")
    "e"  '(:ignore t :wk "eval")
    "g"  '(:ignore t :wk "goto")
    "h"  '(:ignore t :wk "doc")
    "m"  '(:ignore t :wk "macro")
    "s"  '(:ignore t :wk "repl")
    "t"  '(:ignore t :wk "test")

    "'" (if (eq mode 'cider-repl-mode) 'cider-switch-to-last-clojure-buffer 'cider-switch-to-repl-buffer)
    "="  'cider-format-buffer

    "ha" 'cider-apropos
    "hc" 'cider-cheatsheet
    "hg" 'cider-grimoire
    "hh" 'cider-doc
    "hj" 'cider-javadoc
    "hn" 'cider-browse-ns
    "hN" 'cider-browse-ns-all

    "e;" 'cider-eval-defun-to-comment
    "eb" 'cider-eval-buffer
    "el" 'cider-eval-last-sexp
    "ee" 'cider-eval-defun-at-point
    "ep" 'cider-pprint-eval-last-sexp
    "er" 'cider-eval-region
    "er" 'cider-eval-last-sexp-and-replace
    "em" 'cider-macroexpand-1
    "eM" 'cider-macroexpand-all

    "gb" 'cider-pop-back
    "gc" 'cider-classpath
    "ge" 'cider-jump-to-compilation-error
    "gn" 'cider-find-ns
    "gr" 'cider-find-resource
    "gs" 'cider-browse-spec
    "gS" 'cider-browse-spec-all

    "sc" (if (eq mode 'cider-repl-mode) 'cider-repl-clear-buffer 'cider-connect)
    "sC" 'cider-find-and-clear-repl-output
    "so" 'cider-repl-switch-to-other
    "sq" 'cider-quit
    "sr" 'cider-ns-refresh
    "sR" 'cider-restart
    "sh" 'cider-repl-history

    "ta" 'cider-auto-test-mode
    "tr" 'cider-test-show-report

    "dd" 'cider-debug-defun-at-point
    "di" 'cider-inspect
    "dr" 'cider-inspect-last-result

    ;; profile
    "p+" 'cider-profile-samples
    "pc" 'cider-profile-clear
    "pn" 'cider-profile-ns-toggle
    "ps" 'cider-profile-var-summary
    "pS" 'cider-profile-summary
    "pt" 'cider-profile-toggle
    "pv" 'cider-profile-var-profiled-p)
  (defadvice cider-last-sexp (around evil activate)
    "In normal-state or motion-state, last sexp ends at point."
    (if (and (not evil-move-beyond-eol)
	     (or (evil-normal-state-p) (evil-motion-state-p)))
	(save-excursion
	  (unless (or (eobp) (eolp)) (forward-char))
	  ad-do-it)
      ad-do-it)))

(provide 'lain-clojure)
