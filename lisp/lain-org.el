;; lain-org.el -*- lexical-binding: t; -*-

(use-package git)

(use-package org
  :gfhook
  'auto-fill-mode
  'lain/config-org
  ('org-load-hook 'lain/init-org)
  :init
  (defun lain/init-org ()
    (setq-default
     org-hide-leading-stars t
     org-adapt-indentation nil
     org-eldoc-breadcrumb-separator " → "
     org-fontify-done-headline t
     org-fontify-quote-and-verse-blocks t
     org-fontify-whole-heading-line t
     ;; org-startup-folded t
     ;; org-startup-indented t
     org-startup-with-inline-images t
     org-image-actual-width nil
     org-tags-column 0))
  (defun lain//org-update-table ()
    (when (and (org-at-table-p) org-table-may-need-update)
      (save-excursion
	(org-table-recalculate)
	(org-table-align))))
  (defun lain//org-update-statistics ()
    (org-update-statistics-cookies t))
  (defun lain/config-org ()
    (general-add-hook
     '(evil-insert-state-exit-hook evil-replace-state-exit-hook)
     '(lain//org-update-table lain//org-update-statistics)
     nil t)))

(use-package toc-org
  :after org
  :ghook 'org-mode-hook)

(use-package evil-org
  :diminish
  :after org
  :gfhook 'evil-normalize-keymaps
  :ghook 'org-mode-hook
  :init
  (setq
   evil-org-movement-bindings ;; this is how we should do it bois
   '((up    . "n")	      ;; big fricking thank you
     (down  . "t")
     (left  . "h")
     (right . "s"))))

(use-package org-agenda
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (lain/dv-keys 'motion 'org-agenda-mode-map
    "j" "k" "gj" "gk" "C-j" "C-k" "J" "K" "L" "M-j" "M-k" "C-S-l"))

(provide 'lain-org)
