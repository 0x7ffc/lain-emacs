;; init.el -*- lexical-binding: t; -*-

;; SPEEEEED!!

(defvar file-name-handler-alist-old file-name-handler-alist)

(setq file-name-handler-alist nil
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      message-log-max 16384
      package-enable-at-startup nil
      package--init-file-ensured t)

(add-hook 'emacs-startup-hook
          `(lambda ()
	     (message "-----------------------------------------------")
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold 16777216
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

;;Package

(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'lain-package)
(require 'lain-core)
(require 'lain-autoloads)
(require 'lain-keybindings)
(require 'lain-ui)
(require 'lain-theme)
(require 'lain-evil)
(require 'lain-ivy)
(require 'lain-utils)
(require 'lain-emacs-lisp)
