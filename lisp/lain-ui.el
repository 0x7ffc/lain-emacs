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
  :ghook ('prog-mode-hook #'hs-minor-mode))

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

(provide 'lain-ui)
