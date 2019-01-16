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
 inhibit-default-init t
 inhibit-startup-screen t
 inhibit-startup-message t
 initial-major-mode 'text-mode
 ring-bell-function 'ignore
 window-combination-resize t
 sentence-end-double-space nil
 initial-scratch-message nil
 )

(when lain-fullscreen-at-startup
  (add-to-list 'default-frame-alist '(fullscreen . fullboth)))

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
  :diminish)

(use-package rainbow-delimiters
  :general
  (lain-leader-map
   "tr" 'rainbow-delimiters-mode))

(use-package paren-face
  :init
  (setq paren-face-regexp "[][(){}]"))

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

(use-package writeroom-mode
  :general
  (lain-leader-map
   "tw" 'writeroom-mode)
  :init
  (setq
   writeroom-extra-line-spacing 5
   writeroom-width 0.5)
  ;; https://github.com/joostkremers/writeroom-mode/issues/48
  (advice-add 'writeroom--calculate-width :before #'redisplay))

(use-package focus
  :general
  (lain-leader-map
   "tx" 'focus-mode
   "tX" 'focus-read-only-mode))

(use-package centered-cursor-mode
  :general
  (lain-leader-map
   "t-" 'centered-cursor-mode)
  :config
  (setq
   ccm-recenter-at-end-of-file t
   ccm-ignored-commands
   '(mouse-drag-region
     mouse-set-point
     widget-button-click
     scroll-bar-toolkit-scroll
     evil-mouse-drag-region)))

(define-minor-mode lain-zen-mode
  :init-value nil :lighter " Z" :global nil
  (cond
   (lain-zen-mode
    (centered-cursor-mode +1)
    (writeroom-mode +1)
    (focus-mode +1)
    (hl-line-mode -1))
   (t
    (centered-cursor-mode -1)
    (writeroom-mode -1)
    (focus-mode -1)
    (hl-line-mode +1))))

(provide 'lain-ui)
