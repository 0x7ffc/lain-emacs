;; lain-company.el -*- lexical-binding: t; -*-

(use-package company
  :diminish
  :defer .1
  :general
  (company-active-map
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
   company-dabbrev-downcase nil)
  :config
  (global-company-mode +1))

(use-package company-prescient
  :ghook 'company-mode-hook
  :init
  (setq prescient-save-file (no-littering-expand-var-file-name "prescient.el"))
  :config
  (prescient-persist-mode +1))

(provide 'lain-company)
