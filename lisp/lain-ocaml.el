;; lain-ocaml.el -*- lexical-binding: t; -*-

(use-package tuareg
  :init
  (setq tuareg-opam-insinuate t)
  :gfhook 'indent-guide-mode
  :init
  (lain/set-major-mode-leader-keys tuareg-mode
    "m"  '(:ignore t :wk "merlin")
    "d"  '(:ignore t :wk "dune")
    "e"  '(:ignore t :wk "eval"))
  :config
  (tuareg-opam-update-env (tuareg-opam-current-compiler)))

(use-package utop
  :diminish (utop-minor-mode . "")
  :ghook ('tuareg-mode-hook 'utop-minor-mode)
  :general
  ((normal insert)
   utop-mode-map
   "C-t" 'utop-history-goto-next
   "C-n" 'utop-history-goto-prev)
  (emacs
   utop-mode-map
   "C-c C-s" 'lain/ocaml-switch-to-ocaml-buffer)
  (lain-tuareg-mode-map
   "'"  'utop
   "er" 'utop-eval-region
   "ee" 'utop-eval-phrase
   "eb" 'utop-eval-buffer
   "k"  'utop-kill)
  :init
  (defun lain/ocaml-switch-to-ocaml-buffer ()
    (interactive)
    (if (derived-mode-p 'utop-mode)
	(if-let ((buf (--find (with-current-buffer it
				(derived-mode-p 'tuareg-mode))
			      (buffer-list))))
	    (pop-to-buffer-same-window buf)
	  (user-error "No Ocaml buffer found"))
      (user-error "Not in a utop REPL buffer")))
  :config
  (evil-set-initial-state 'utop-mode 'emacs)
  (setq utop-protocol-version "1")
  (setq utop-command "opam config exec -- dune utop . -- -emacs")
  (lain/set-popup-rules '("^\\*utop\\*$" :quit nil :size 0.3)))

(use-package dune
  :general
  (lain-tuareg-mode-map
   "dp" 'dune-promote
   "dt" 'dune-runtest-and-promote
   "dF" 'lain/ocaml-format-project)
  :init
  (defun lain/ocaml-format-project ()
    (interactive)
    (y-or-n-p
     (async-shell-command "dune build @fmt --auto-promote"))))

(use-package merlin
  :ghook 'tuareg-mode-hook
  :general
  ((normal insert)
   merlin-mode-map
   "M-." 'merlin-locate
   "M-," 'merlin-pop-stack)
  (lain-tuareg-mode-map
   "mr" 'merlin-occurrences
   "md" 'merlin-document
   "mt" 'merlin-type-enclosing)
  :init
  (setq merlin-completion-with-doc t)
  :config
  (after tuareg
    (lain/set-company-backend '(tuareg-mode) 'merlin-company-backend))
  (lain/set-popup-rules '("^\\*merlin-types\\*$" :size 0.3 :slot -1)))

(use-package merlin-eldoc
  :after merlin
  :ghook ('merlin-mode-hook 'merlin-eldoc-setup))

(use-package ocp-indent
  :gfhook ('tuareg-mode-hook 'ocp-setup-indent)
  :general
  (lain-tuareg-mode-map
   "=" 'ocp-indent-buffer))

(provide 'lain-ocaml)
