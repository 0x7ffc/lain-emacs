;; lain-ivy.el -*- lexical-binding: t; -*-

(use-package ivy
  :diminish
  :general
  ([remap switch-to-buffer] 'ivy-switch-buffer)
  (ivy-minibuffer-map
   "C-s"   'ivy-alt-done
   "C-M-s" 'ivy-immediate-done
   "C-t"   'ivy-next-line
   "C-n"   'ivy-previous-line
   "C-u"   'ivy-scroll-down-command
   "C-d"   'ivy-scroll-up-command
   "M-t"   'ivy-next-history-element
   "M-n"   'ivy-previous-history-element)
  (ivy-occur-mode-map
   "D"  'ivy-occur-delete-candidate
   "t"  'ivy-occur-next-line
   "n"  'ivy-occur-previous-line
   "h"  'evil-backward-char
   "s"  'evil-forward-char
   "g"  nil
   "gg" 'evil-goto-first-line
   "gf" 'ivy-occur-press
   "ga" 'ivy-occur-read-action
   "go" 'ivy-occur-dispatch
   "gc" 'ivy-occur-toggle-calling
   "gr" 'ivy-occur-revert-buffer
   "q"  'quit-window)
  (lain-leader-map
   "bb" 'ivy-switch-buffer)
  :init
  (setq
   ivy-wrap t
   ivy-extra-directories nil
   ivy-use-virtual-buffers t
   ivy-initial-inputs-alist nil
   ivy-on-del-error-function nil
   projectile-completion-system 'ivy
   ivy-format-function #'ivy-format-function-line)
  :config
  (evil-set-initial-state 'ivy-occur-grep-mode 'normal)
  (ivy-mode 1))

(use-package swiper
  :general
  ((normal visual)
   "*"   'lain/swiper-at-point
   "M-*" 'lain/swiper-all-at-point)
  :config
  (defun lain/swiper-at-point ()
    (interactive)
    (swiper (lain/symbol-at-point)))
  (defun lain/swiper-all-at-point ()
    (interactive)
    (swiper-all (lain/symbol-at-point))))

(use-package counsel
  :general
  ([remap find-library]             'counsel-find-library
   [remap find-file]                'counsel-find-file
   [remap isearch-forward]          'counsel-grep-or-swiper
   [remap info-lookup-symbol]       'counsel-info-lookup-symbol
   [remap describe-function]        'counsel-describe-function
   [remap describe-variable]        'counsel-describe-variable
   [remap describe-bindings]        'counsel-descbinds
   [remap execute-extended-command] 'counsel-M-x
   [remap imenu]                    'counsel-imenu)
  (counsel-find-file-map
   "C-h" 'counsel-up-directory)
  (lain-leader-map
   ","   'counsel-M-x
   "/"   'counsel-rg
   "*"   'lain/counsel-rg-at-point
   "fr"  'counsel-recentf
   "ff"  'counsel-find-file
   "ji"  'counsel-imenu
   "el"  'counsel-find-library
   "hf"  'counsel-describe-function
   "hv"  'counsel-describe-variable
   "hb"  'counsel-descbinds
   "hS"  'counsel-info-lookup-symbol)
  :config
  (defun lain/counsel-rg-at-point ()
    (interactive)
    (require 'projectile)
    (counsel-rg (lain/symbol-at-point)
		(projectile-ensure-project (projectile-project-root)))))

(use-package counsel-projectile
  :after projectile
  :general
  ([remap projectile-find-file]        #'counsel-projectile-find-file
   [remap projectile-find-dir]         #'counsel-projectile-find-dir
   [remap projectile-switch-to-buffer] #'counsel-projectile-switch-to-buffer
   [remap projectile-ripgrep]          #'counsel-projectile-rg
   [remap projectile-switch-to-buffer] #'counsel-projectile-switch-to-buffer
   [remap projectile-switch-project]   #'counsel-projectile-switch-project))

(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :config
  (setq wgrep-auto-save-buffer t))

(provide 'lain-ivy)
