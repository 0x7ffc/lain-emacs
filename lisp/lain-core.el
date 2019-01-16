;; lain-core.el -*- lexical-binding: t; -*-

(prefer-coding-system 'utf-8)
(fset #'yes-or-no-p #'y-or-n-p)
(electric-indent-mode -1)
(setq
 inhibit-compacting-font-caches t
 auto-save-default nil
 create-lockfiles nil
 make-backup-files nil
 indent-tabs-mode nil
 auto-window-vscroll nil
 vc-follow-symlinks t
 echo-keystrokes 0.02
 find-file-visit-truename t
 hscroll-step 1
 hscroll-margin 2
 scroll-margin 0
 scroll-conservatively 100000
 scroll-preserve-screen-position t)

(use-package s
  :demand t)

(use-package dash
  :demand t)

(use-package f
  :demand t)

(use-package general
  :demand t
  :config
  (general-create-definer lain-leader-def
    :keymaps '(normal visual insert emacs)
    :prefix lain-leader-key
    :prefix-map 'lain-leader-map
    :non-normal-prefix lain-emacs-leader-key))

(use-package no-littering
  :demand t
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :demand t
    :config
    (exec-path-from-shell-initialize)))

(use-package which-key
  :defer .1
  :diminish
  :general
  (which-key-C-h-map
   "q" 'which-key-abort
   "t" 'which-key-show-next-page-cycle
   "n" 'which-key-show-previous-page-cycle
   "?" 'which-key-show-standard-help)
  :init
  (setq
   which-key-idle-delay 0.3
   which-key-enable-extended-define-key t)
  :config
  (add-hook 'which-key-init-buffer-hook
	    (lambda () (setq-local line-spacing 3)) t)
  (which-key-mode +1))

(use-package projectile
  :diminish
  :defer .5
  :general
  (lain-leader-map
   "pp" 'projectile-switch-project
   "pI" 'projectile-invalidate-cache
   "pC" 'projectile-cleanup-known-projects
   "pf" 'projectile-find-file
   "pd" 'projectile-find-dir
   "pD" 'lain/projectile-deer
   "pk" 'projectile-kill-buffers
   "pb" 'projectile-switch-to-buffer)
  :init
  (when (executable-find "fd")
    (setq
     projectile-git-command "fd . -t f -0 -c never"
     projectile-generic-command projectile-git-command))
  (setq
   projectile-enable-caching t
   projectile-sort-order 'recently-active
   projectile-require-project-root t
   projectile-buffers-filter-function 'projectile-buffers-with-file-or-process
   projectile-kill-buffers-filter 'kill-only-files)
  :config
  (defun lain/projectile-deer ()
    "Open `deer' from the root of the project."
    (interactive)
    (deer (projectile-ensure-project (projectile-project-root))))
  (projectile-mode +1))

(use-package smartparens
  :diminish
  :ghook 'eval-expression-minibuffer-setup-hook
  :init
  (setq
   sp-highlight-pair-overlay nil
   sp-highlight-wrap-overlay nil
   sp-highlight-wrap-tag-overlay nil
   sp-cancel-autoskip-on-backward-movement nil
   sp-show-pair-from-inside t
   sp-show-pair-delay 0.1
   sp-max-pair-length 4
   sp-max-prefix-length 50
   sp-escape-quotes-after-insert nil)
  :config
  (require 'smartparens-config)
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (show-smartparens-global-mode +1))

(use-package avy
  :general
  (lain-leader-map
   "jp"  'avy-pop-mark
   "SPC" 'evil-avy-goto-char-timer
   "jl"  'evil-avy-goto-line
   "jw"  'evil-avy-goto-word-or-subword-1)
  :init
  (setq
   avy-background t
   avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s)))

(use-package whitespace-cleanup-mode
  :defer 1
  :diminish
  :commands whitespace-cleanup-mode
  :config
  (global-whitespace-cleanup-mode 1))

(use-package esup
  :commands (esup esup-next-result esup-previous-result)
  :general
  (normal
   esup-mode-map
   "q"   'quit-window
   "C-t" 'esup-next-result
   "C-n" 'esup-previous-result))

(use-package ace-window
  :general
  (lain-leader-map
   "ww" 'ace-window
   "wD" 'ace-delete-window
   "wS" 'ace-swap-window)
  :init
  (setq
   aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s)
   aw-scope 'frame
   aw-background t))

(use-package winum
  :general
  (lain-leader-map
   "0" 'winum-select-window-0-or-10
   "1" 'winum-select-window-1
   "2" 'winum-select-window-2
   "3" 'winum-select-window-3
   "4" 'winum-select-window-4
   "5" 'winum-select-window-5
   "6" 'winum-select-window-6
   "7" 'winum-select-window-7
   "8" 'winum-select-window-8
   "9" 'winum-select-window-9)
  :init
  (setq
   winum-auto-assign-0-to-minibuffer nil
   winum-auto-setup-mode-line nil
   winum-ignored-buffers '(" *which-key*"))
  ;; From spacemacs, group which-key descriptions
  (after which-key
    (push '(("\\(.*\\)0" . "winum-select-window-0-or-10") .
	    ("\\10" . "select window 0 or 10"))
	  which-key-replacement-alist)
    (push '(("\\(.*\\)1" . "winum-select-window-1") .
	    ("\\11..9" . "select window 1..9"))
	  which-key-replacement-alist)
    (push '((nil . "winum-select-window-[2-9]") . t)
	  which-key-replacement-alist))
  :config
  (winum-mode))

(use-feature vc
  :demand t
  :general
  (normal
   vc-annotate-mode-map
   "C-t" 'vc-annotate-next-revision
   "C-n" 'vc-annotate-prev-revision
   "q"   'quit-window)
  (lain-leader-map
   "ga" 'vc-annotate)
  :config
  (after evil
    (evil-set-initial-state 'vc-annotate-mode 'normal))
  (remove-hook 'find-file-hook 'vc-find-file-hook)
  (setq vc-handled-backends '(Git)))

(use-feature autorevert
  :demand t
  :config
  (setq
   auto-revert-verbose nil
   global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode 1))

(use-feature saveplace
  :demand t
  :config
  (save-place-mode +1))

(use-feature recentf
  :config
  (setq
   recentf-max-saved-items 100
   recentf-exclude
   (list #'file-remote-p
	 ;; exclude straight.el files
	 (lambda (file) (file-in-directory-p file "~/.emacs.d/straight"))
	 "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$"
	 "^/tmp/" "^/ssh:"))
  (let ((inhibit-message t))
    (recentf-mode 1)))

(use-package ibuffer
  :general
  ([remap list-buffers] 'ibuffer)
  (lain-leader-map
   "bB" 'ibuffer)
  :config
  (evil-ex-define-cmd "buffers" 'ibuffer))

(use-package ibuffer-projectile
  :after ibuffer
  :gfhook ('ibuffer-hook 'lain//ibuffer-sort-by-project)
  :init
  (defun lain//ibuffer-sort-by-project ()
    (ibuffer-projectile-set-filter-groups)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic))))

(use-package prescient
  :defer .5
  :init
  (setq prescient-save-file (no-littering-expand-var-file-name "prescient.el"))
  :config
  (prescient-persist-mode +1))

(use-feature winner
  :defer .5
  :commands (winner-undo winner-redo)
  :preface (defvar winner-dont-bind-my-keys t)
  :general
  (lain-leader-map
   "wu" 'winner-undo
   "wr" 'winner-redo)
  :config
  (winner-mode +1))

(use-feature xref
  :general
  (normal
   xref--xref-buffer-mode-map
   "q"   'quit-window
   "C-t" 'xref-next-line
   "C-n" 'xref-prev-line
   "o"   'xref-show-location-at-point))

(use-feature eldoc
  :diminish
  :ghook
  'eval-expression-minibuffer-setup-hook
  'ielm-mode-hook)

(use-package help-fns+
  :general
  (lain-leader-map
   "hK" 'describe-keymap))

(use-package pcre2el)

(use-package helpful
  :defer .5
  :general
  ([remap describe-key]      'helpful-key
   [remap describe-variable] 'helpful-variable
   [remap describe-command]  'helpful-command
   [remap describe-function] 'helpful-function))

(provide 'lain-core)
