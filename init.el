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
	     (unless lain-inhibit-startup-time
	       (message "Emacs ready in %s with %d garbage collections."
			(format "%.2f seconds"
				(float-time
				 (time-subtract after-init-time before-init-time)))
			gcs-done))
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold 16777216
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

;; Packages

(defvar lain-emacs-dir
  (or (let ((alternate-dir (getenv "USER_EMACS_DIRECTORY")))
	(when alternate-dir
	  (file-truename alternate-dir)))
      (file-truename user-emacs-directory))
  "Path to Emacs configuration directory
Use environment variable `USER_EMACS_DIR' when it's avaliable
Otherwise it's `/home/$USER/.emacs.d/'")

(setq
 user-emacs-directory lain-emacs-dir
 lain-init-file (expand-file-name "init.el" lain-emacs-dir)
 lain-lisp-dir (concat lain-emacs-dir "lisp/")
 lain-personal-dir (concat lain-emacs-dir "personal/")
 lain-main-file (expand-file-name "lain.el" lain-personal-dir))

(if (file-exists-p lain-main-file)
    (load-file lain-main-file)
  (message (concat "There is no lain.el inside personal directory."
		   "See lain.el.example for an example of it.")))

(lain/init)
(lain/user-init)

(add-to-list 'load-path lain-lisp-dir)
(require 'lain-package)
(require 'lain-core)
(require 'lain-autoloads)
(require 'lain-keybindings)
(require 'lain-ui)
(require 'lain-theme)
(require 'lain-evil)
(require 'lain-ivy)

(-each lain-modules 'require)

(lain/user-config)

(-each (-remove-item lain-main-file (directory-files-recursively "~/.emacs.d/personal" "\\.el$")) 'load-file)
