;; lain-ui.el -*- lexical-binding: t; -*-

;; Put this into ~/.Xresources:
;;   Emacs*toolBar: false
;;   Emacs*menuBar: false
;;   Emacs*.verticalScrollBars: false
;; Then run: xrdb ~/.Xresources
(tooltip-mode -1)
(when tool-bar-mode
  (tool-bar-mode -1))
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(fset #'display-startup-echo-area-message #'ignore)
(setq
 default-frame-alist '((tool-bar-lines           . 0)
		       (menu-bar-lines           . 0)
		       (inhibit-double-buffering . t)
		       (vertical-scroll-bars     . nil)
		       (horizontal-scroll-bars   . nil))
 enable-recursive-minibuffers t
 eval-expression-print-length nil
 eval-expression-print-level nil
 help-window-select 't
 inhibit-default-init t
 inhibit-startup-screen t
 inhibit-startup-message t
 initial-major-mode 'text-mode
 ring-bell-function 'ignore
 window-combination-resize t
 sentence-end-double-space nil
 initial-scratch-message nil
 )

(defun lain/set-default-font (font)
  (when (find-font (font-spec :name (car font)))
    (let* ((font-name (car font))
	   (props (cdr font))
	   (fontspec (apply 'font-spec :name font-name props)))
      (add-to-list 'default-frame-alist
		   (cons 'font (font-xlfd-name fontspec))))))
(lain/set-default-font lain-font)

(when lain-fullscreen-at-startup
  (add-to-list 'default-frame-alist '(fullscreen . fullboth)))

(use-feature lain-fira-code
  :diminish
  :ghook
  'prog-mode-hook
  'text-mode-hook
  :if (or (boundp 'mac-auto-operator-composition-mode)
	  (s-equals? "Fira Code"
		     (alist-get 'font default-frame-alist))))

;; Enable evil-mode's code folding feature: `zm' `zr'
(use-feature hideshow
  :diminish hs-minor-mode
  :ghook ('prog-mode-hook 'hs-minor-mode))

(use-feature hl-line
  :ghook
  'prog-mode-hook
  'text-mode-hook
  :init
  (setq hl-line-sticky-flag nil)
  :config
  ;; Toggle hl-line when entering and exiting evil visual state
  (after evil
    (defvar-local was-hl-line-mode-on nil)
    (defun hl-line-on-maybe ()  (if was-hl-line-mode-on (hl-line-mode +1)))
    (defun hl-line-off-maybe () (if was-hl-line-mode-on (hl-line-mode -1)))
    (add-hook 'hl-line-mode-hook
	      (lambda () (if hl-line-mode (setq was-hl-line-mode-on t))))
    (add-hook 'evil-visual-state-entry-hook 'hl-line-off-maybe)
    (add-hook 'evil-visual-state-exit-hook 'hl-line-on-maybe)))

(use-package highlight-parentheses
  :diminish
  :ghook 'prog-mode-hook)

(use-package rainbow-delimiters
  :general
  (lain-leader-map
   "tr" 'rainbow-delimiters-mode))

(use-package paren-face
  :ghook 'emacs-lisp-mode-hook)

(use-package hl-todo
  :ghook
  'text-mode-hook
  'prog-mode-hook)

(use-package page-break-lines
  :diminish
  :ghook
  'text-mode-hook
  'prog-mode-hook)

(use-package all-the-icons
  :commands (all-the-icons-octicon
	     all-the-icons-faicon
	     all-the-icons-fileicon
	     all-the-icons-wicon
	     all-the-icons-material
	     all-the-icons-alltheicon
	     all-the-icons-install-fonts))

(use-package doom-modeline
  :ghook ('after-init-hook 'doom-modeline-init)
  :init
  (setq
   ;;doom-modeline-height 25
   doom-modeline-major-mode-icon nil
   doom-modeline-bar-width 3
   doom-modeline-buffer-file-name-style 'relative-to-project
   doom-modeline-minor-modes t
   doom-modeline-github nil))

(use-package shackle
  :defer .5
  :init
  (setq
   shackle-rules
   '((compilation-mode :select nil)
     (" *undo-tree*" :size 0.25  :align right)
     ("*eshell*" :select t :other t)
     ("*Shell Command Output*" :select nil)
     ("\\*Async Shell.*\\*" :regexp t :ignore t)
     ("*Help*" :select t :other t)
     ("*Completions*" :size 0.3 :align t)
     ("*Messages*" :select nil :other t)
     ("\\*[Wo]*Man.*\\*" :regexp t :select t :other t)
     ("\\*poporg.*\\*" :regexp t :select t :other t)
     ("*Calendar*" :select t :size 0.3 :align below)
     ("*info*" :select t :inhibit-window-quit t :same t)
     (ivy-occur-grep-mode :select t :align t)
     (magit-status-mode :select t :inhibit-window-quit t :same t)
     (magit-log-mode :select t :inhibit-window-quit t :same t)
     ))
  :config
  (shackle-mode +1))

(use-package writeroom-mode
  :general
  (lain-leader-map
   "tw" 'writeroom-mode))

(provide 'lain-ui)
