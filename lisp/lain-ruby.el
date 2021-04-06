;; lain-ruby.el -*- lexical-binding: t; -*-

(defvar lain-ruby-fns
  '(smartparens-strict-mode
    evil-cleverparens-mode
    highlight-parentheses-mode
    indent-guide-mode
    subword-mode)
  "Functions to run for all ruby modes")

(use-package enh-ruby-mode
  :gfhook lain-ruby-fns
  :mode
  (("Appraisals\\'" . enh-ruby-mode)
   ("\\(Rake\\|Thor\\|Guard\\|Gem\\|Cap\\|Vagrant\\|Berks\\|Pod\\|Puppet\\)file\\'" . enh-ruby-mode)
   ("\\.\\(rb\\|rabl\\|ru\\|builder\\|rake\\|thor\\|gemspec\\|jbuilder\\)\\'" . enh-ruby-mode))
  :interpreter "ruby"
  :general
  ((normal insert)
   inf-ruby-mode-map
   "C-t" 'comint-next-input
   "C-n" 'comint-previous-input
   "C-r" 'comint-history-isearch-backward-regexp)
  :config
  (after company-dabbrev-code
    (add-to-list 'company-dabbrev-code-modes 'enh-ruby-mode)))

(use-package robe
  :diminish
  :ghook
  'enh-ruby-mode-hook
  ('inf-ruby-mode-hook 'smartparens-mode)
  :general
  ((normal insert)
   robe-mode-map
   "M-." 'robe-jump)
  ((normal insert)
   inf-ruby-minor-mode-map
   "C-c C-e" 'ruby-send-definition
   "C-c M-e" 'ruby-send-definition-and-go
   "C-c C-l" 'ruby-send-line
   "C-c M-l" 'ruby-send-line-and-go)
  :config
  (lain/set-popup-rules
    '("^\\*ruby\\*$" :quit nil :select t :size 0.3)
    '("^\\*pry\\*$" :quit nil :size 0.3)
    '("^\\*robe-doc\\*$" :select t :slot 1 :size 0.3))
  (lain/set-major-mode-leader-keys (inf-ruby-mode enh-ruby-mode)
    "'" 'robe-start
    "e"  '(:ignore t :wk "eval")
    "ee" 'ruby-send-definition
    "eE" 'ruby-send-definition-and-go
    "eb" 'ruby-send-buffer
    "eB" 'ruby-send-buffer-and-go
    "el" 'ruby-send-line
    "eL" 'ruby-send-line-and-go
    "er" 'ruby-send-region
    "eR" 'ruby-send-region-and-go)
  (lain/set-company-backend '(enh-ruby-mode inf-ruby-mode) 'company-robe))

(use-package rbenv
  :ghook ('enh-ruby-mode-hook 'lain/enable-rbenv)
  :init
  (setq rbenv-show-active-ruby-in-modeline nil)
  (defun lain/enable-rbenv ()
    (require 'rbenv)
    (let ((version-file-path (rbenv--locate-file ".ruby-version")))
      (global-rbenv-mode)
      ;; try to use the ruby defined in .ruby-version
      (if version-file-path
	  (progn
	    (rbenv-use (rbenv--read-version-from-file
			version-file-path))
	    (message (concat "[rbenv] Using ruby version "
			     "from .ruby-version file.")))
	(message "[rbenv] Using the currently activated ruby.")))))

(use-package bundler
  :after enh-ruby-mode
  :init
  (lain/set-major-mode-leader-keys (inf-ruby-mode enh-ruby-mode)
    "b"  '(:ignore t :wk "bundler")
    "bc" 'bundle-check
    "bi" 'bundle-install
    "bs" 'bundle-console
    "bu" 'bundle-update
    "bx" 'bundle-exec
    "bo" 'bundle-open)
  (lain/set-popup-rules
    '("^\\*Bundler\\*$" :size 0.3 :slot 1)))

(use-package seeing-is-believing
  :diminish (seeing-is-believing . "")
  :commands
  (seeing-is-believing seeing-is-believing-run seeing-is-believing-clear)
  :ghook ('enh-ruby-mode-hook 'seeing-is-believing)
  :init
  (lain/set-major-mode-leader-keys (inf-ruby-mode enh-ruby-mode)
    "m"  '(:ignore t :wk "seeing")
    "mb" 'seeing-is-believing-run
    "mc" 'seeing-is-believing-clear))

(provide 'lain-ruby)
