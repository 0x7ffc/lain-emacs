;; lain-company.el -*- lexical-binding: t; -*-

(use-package company
  :diminish
  :defer .5
  :general
  (company-active-map
   "TAB" 'company-complete-selection
   "<tab>" 'company-complete-selection
   "RET" 'nil
   [return] 'nil
   "C-t" 'company-select-next
   "C-n" 'company-select-previous
   "C-u" 'company-previous-page
   "C-d" 'company-next-page)
  :init
  (setq
   company-require-match nil
   company-idle-delay 0.1
   company-tooltip-idle-delay 0.1
   company-tooltip-align-annotations nil
   company-minimum-prefix-length 2
   company-dabbrev-code-other-buffers t
   company-dabbrev-ignore-case nil
   company-dabbrev-downcase nil
   company-backends '((company-capf company-files company-dabbrev-code))
   company-frontends
   '(company-pseudo-tooltip-frontend
     company-echo-metadata-frontend))
  (defun lain/set-company-backend (mode backend)
    (add-hook (intern (format "%s-hook" mode))
	      (lambda ()
		(set (make-local-variable 'company-backends)
		     (if (consp backend)
			 (append backend (default-value 'company-backends))
		       (cons backend (default-value 'company-backends)))))))
  :config
  (global-company-mode +1))

(use-package company-prescient
  :ghook 'company-mode-hook)

(provide 'lain-company)
