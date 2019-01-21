;; lain-clojure.el -*- lexical-binding: t; -*-

(defvar lain-clojure-fns
  '(smartparens-strict-mode
    evil-cleverparens-mode
    highlight-parentheses-mode
    paren-face-mode
    indent-guide-mode
    subword-mode)
  "Functions to run for all clojure modes")

(defvar lain-clojure-major-modes
  '(clojure-mode
    clojurec-mode
    clojurescript-mode
    cider-repl-mode
    cider-clojure-interaction-mode)
  "All clojure related modes")

(use-package clojure-mode
  :gfhook lain-clojure-fns
  :config
  (lain/set-popup-rules
    '("^\\*cider-error" :size 0.3 :slot 1)
    '("^\\*cider-repl" :size 0.3 :quit nil :select t)
    '("^\\*cider-repl-history" :size 0.4 :side right :slot 2)))

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
  (lain/set-major-mode-leader-keys clojure-mode
    "'" 'cider-jack-in)
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
  (defadvice cider-last-sexp (around evil activate)
    "In normal-state or motion-state, last sexp ends at point."
    (if (and (not evil-move-beyond-eol)
	     (or (evil-normal-state-p) (evil-motion-state-p)))
	(save-excursion
	  (unless (or (eobp) (eolp)) (forward-char))
	  ad-do-it)
      ad-do-it)))

(provide 'lain-clojure)
