;; lain-ocaml.el -*- lexical-binding: t; -*-

(defvar lain-ocaml-fn
  '(smartparens-mode
    evil-cleverparens-mode
    highlight-parentheses-mode
    indent-guide-mode
    subword-mode))

(use-package tuareg
  :init
  (setq tuareg-opam-insinuate t)
  :gfhook lain-ocaml-fn
  :init
  (lain/set-major-mode-leader-keys tuareg-mode
    "m"  '(:ignore t :wk "merlin")
    "d"  '(:ignore t :wk "dune")
    "e"  '(:ignore t :wk "eval"))
  :config
  (after smartparens
    (sp-with-modes '(tuareg-mode)
      (sp-local-pair "struct" "end"
		     :unless '(sp-in-comment-p sp-in-string-p)
		     :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
      (sp-local-pair "sig" "end"
		     :unless '(sp-in-comment-p sp-in-string-p)
		     :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
      (sp-local-pair "if" "then"
		     :unless '(sp-in-comment-p sp-in-string-p)
		     :post-handlers '(("| " "SPC")))
      (sp-local-pair "while" "done"
		     :unless '(sp-in-comment-p sp-in-string-p)
		     :post-handlers '(("| " "SPC")))
      (sp-local-pair "for" "done"
		     :unless '(sp-in-comment-p sp-in-string-p)
		     :post-handlers '(("| " "SPC")))
      (sp-local-pair "begin" "end"
		     :unless '(sp-in-comment-p sp-in-string-p)
		     :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
      (sp-local-pair "object" "end"
		     :unless '(sp-in-comment-p sp-in-string-p)
		     :post-handlers '(("| " "SPC")))
      (sp-local-pair "match" "with"
		     :actions '(insert)
		     :unless '(sp-in-comment-p sp-in-string-p)
		     :post-handlers '(("| " "SPC")))
      (sp-local-pair "try" "with"
		     :actions '(insert)
		     :unless '(sp-in-comment-p sp-in-string-p)
		     :post-handlers '(("| " "SPC")))
      ))
  (tuareg-opam-update-env (tuareg-opam-current-compiler)))

(use-package utop
  :diminish (utop-minor-mode . "")
  :ghook ('tuareg-mode-hook 'utop-minor-mode)
  :general
  (emacs
   utop-mode-map
   "M-."     'merlin-locate
   "M-,"     'merlin-pop-stack
   "C-c C-s" 'lain/ocaml-switch-to-ocaml-buffer)
  ((normal insert)
   utop-minor-mode-map
   "C-c C-s" 'lain/ocaml-utop)
  (lain-tuareg-mode-map
   "'"  'lain/ocaml-utop
   "er" 'utop-eval-region
   "ee" 'utop-eval-phrase
   "eb" 'utop-eval-buffer
   "k"  'lain/ocaml-utop-kill)
  :init
  (setq
   utop-protocol-version "1"
   utop-command "opam config exec -- dune utop . -- -emacs"
   utop-edit-command nil)
  (defun lain/ocaml-utop-kill ()
    (interactive)
    (utop-kill)
    (kill-buffer utop-buffer-name))
  (defun lain/ocaml-utop ()
    (interactive)
    (let ((buf (get-buffer utop-buffer-name)))
      (if (or (not buf) (with-current-buffer buf (eq utop-state 'done)))
	  (let ((default-directory (projectile-expand-root
				    (read-string "Directory: " "."))))
	    (message default-directory)
	    (utop))
	(utop))))
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
  (lain/set-popup-rules '("^\\*utop\\*$" :quit nil :size 0.3)))

(use-package dune
  :mode (("dune-project\\'" . dune-mode))
  :general
  (lain-tuareg-mode-map
   "db" 'lain/ocaml-build-project
   "dc" 'lain/ocaml-clean-project
   "dp" 'dune-promote
   "dt" 'dune-runtest-and-promote
   "dF" 'lain/ocaml-format-project)
  :init
  (defun lain/ocaml-build-project ()
    (interactive)
    (compile "dune build"))
  (defun lain/ocaml-clean-project ()
    (interactive)
    (compile "dune clean"))
  (defun lain/ocaml-format-project ()
    (interactive)
    (when (y-or-n-p "Format now?")
      (compile "dune build @fmt --auto-promote"))))

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
