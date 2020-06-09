; lain-package.el -*- lexical-binding: t; -*-

(require 'package)

(setq
 package-enable-at-startup nil
 load-prefer-newer t
 use-package-always-defer t
 use-package-expand-minimally t
 use-package-verbose nil
 ;; use-package-compute-statistics t
 ;; use-package-expand-minimally nil
 ;; debug-on-error t
 )

(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("org"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package diminish)

(defmacro after (name &rest args)
  "Simple wrapper around `with-eval-after-load'"
  `(with-eval-after-load ',name ,@args))
(put 'after 'lisp-indent-function 'defun)

(provide 'lain-package)
