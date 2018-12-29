;; lain-utils.el -*- lexical-binding: t; -*-

(use-package expand-region
  :config
  (defun evil-visual-char-or-expand-region ()
    (interactive)
    (if (region-active-p)
	(call-interactively 'er/expand-region)
      (evil-visual-char)))
  :general
  (normal "v" 'evil-visual-char-or-expand-region)
  (visual "v" 'evil-visual-char-or-expand-region)
  (visual [escape] 'evil-visual-char))

(use-package ranger
  :commands (ranger deer ranger-override-dired-mode)
  :general
  (normal "-" 'deer)
  (lain-leader-map
   "ar" 'ranger)
  :config
  (lain/dv-keys nil 'ranger-mode-map
    "j" "k" "l")
  (ranger-override-dired-mode t))

(use-package magit
  :general
  (lain-leader-map
   "gs" 'magit-status
   "gi" 'magit-init)
  (magit-status-mode-map
   "Q" 'lain/magit-kill-buffers)
  :init
  (setq
   magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
   magit-completing-read-function #'ivy-completing-read)
  :config
  (magit-change-popup-key 'magit-dispatch-popup :actions ?t ?j)

  (defun lain/magit-kill-buffers ()
    "Restore window configuration and kill all Magit buffers."
    (interactive)
    (let ((buffers (magit-mode-get-buffers)))
      (magit-restore-window-configuration)
      (-each buffers 'kill-buffer))))

(use-package evil-magit
  :demand t
  :after (evil magit)
  :config
  (lain/dv-keys `(,evil-magit-state visual) 'magit-mode-map
    "j" "k" "C-j" "C-k"))

(provide 'lain-utils)
