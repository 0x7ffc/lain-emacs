;; lain-org.el -*- lexical-binding: t; -*-

;;; Prevent Emacs-provided Org from being loaded

;; The following is a temporary hack until straight.el supports
;; building Org, see:
;;
;; * https://github.com/raxod502/straight.el/issues/211
;; * https://github.com/raxod502/radian/issues/410
;;
;; There are three things missing from our version of Org: the
;; functions `org-git-version' and `org-release', and the feature
;; `org-version'. We provide all three of those ourself, therefore.

;; Package `git' is a library providing convenience functions for
;; running Git.
(use-package git)

;;;###autoload
(defun org-git-version ()
  "The Git version of `org-mode'.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (f-expand
		   "straight/repos/org/" user-emacs-directory)))
    (s-trim
     (git-run "describe"
	      "--match=release\*"
	      "--abbrev=6"
	      "HEAD"))))

;;;###autoload
(defun org-release ()
  "The release version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (f-expand
		   "straight/repos/org/" user-emacs-directory)))
    (s-trim
     (s-chop-prefix
      "release_"
      (git-run "describe"
	       "--match=release\*"
	       "--abbrev=0"
	       "HEAD")))))

(provide 'org-version)

(use-package org
  :straight org-plus-contrib
  :lain-major-mode
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

(use-package org-bullets
  :after org
  :straight (:host github :repo "Kaligule/org-bullets")
  :ghook 'org-mode-hook)

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

(use-feature org-agenda
  :gfhook
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (lain/dv-keys 'motion 'org-agenda-mode-map
    "j" "k" "gj" "gk" "C-j" "C-k" "J" "K" "L" "M-j" "M-k" "C-S-l"))

(provide 'lain-org)
