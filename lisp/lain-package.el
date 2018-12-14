; lain-package.el -*- lexical-binding: t; -*-

(setq
 load-prefer-newer t
 straight-repository-branch "develop"
 straight-check-for-modifications '(find-when-checking)
 straight-use-package-by-default t
 straight-cache-autoloads t
 straight-treat-as-init t
 use-package-always-defer t
 use-package-verbose nil
 ;; use-package-compute-statistics t
 ;; use-package-expand-minimally nil
 ;; debug-on-error t
 )

(add-hook 'after-init-hook `(lambda ()
			      (setq straight-treat-as-init nil)
			      (straight-finalize-transaction)))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(use-package diminish :demand t)

(eval-when-compile
  (setq use-package-expand-minimally byte-compile-current-file))

(defmacro use-feature (name &rest args)
  "Like `use-package', but with `straight-use-package-by-default' disabled."
  `(use-package ,name
     :straight nil
     ,@args))
(put 'use-feature 'lisp-indent-function 'defun)

(defmacro after (name &rest args)
  "Simple wrapper around `with-eval-after-load'"
  `(with-eval-after-load ',name ,@args))
(put 'after 'lisp-indent-function 'defun)

(provide 'lain-package)
