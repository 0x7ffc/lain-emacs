;; lain-ocaml.el -*- lexical-binding: t; -*-

(use-package tuareg
  :init
  (setq tuareg-opam-insinuate t)
  :gfhook 'indent-guide-mode
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
  :config
  (lain/set-major-mode-leader-keys tuareg-mode
    "'"  'utop
    "m"  '(:ignore t :wk "merlin")
    "d"  '(:ignore t :wk "dune")
    "e"  '(:ignore t :wk "eval")
    "er" 'utop-eval-region
    "ee" 'utop-eval-phrase
    "eb" 'utop-eval-buffer
    "k"  'utop-kill)
  (setq utop-command "opam config exec -- utop -emacs")
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
   "M-." 'merlin-locate)
  (lain-tuareg-mode-map
   "mr" 'merlin-occurrences
   "md" 'merlin-document
   "mt" 'merlin-type-enclosing)
  :init
  (setq merlin-completion-with-doc t)
  (after tuareg
    (lain/set-company-backend '(tuareg-mode) 'merlin-company-backend)))

(use-package merlin-eldoc
  :after merlin
  :ghook ('merlin-mode-hook 'merlin-eldoc-setup))

(use-package ocp-indent
  :gfhook ('tuareg-mode-hook 'ocp-setup-indent)
  :general
  (lain-tuareg-mode-map
   "=" 'ocp-indent-buffer))

(provide 'lain-ocaml)
