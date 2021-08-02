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

(when lain-behind-GFW
  (setq package-archives
	'(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
          ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
          ("org"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/"))))
(package-initialize)

(when lain-proxy-addr
  (setq url-proxy-services
	`(("no_proxy" . "^\\(localhost\\|10.*\\)")
	  ("http" . ,lain-proxy-addr)
	  ("https" . ,lain-proxy-addr))))

(when lain-proxy-username
  (setq url-http-proxy-basic-auth-storage
	(list (list lain-proxy-addr
		    (cons lain-proxy-username
			  (base64-encode-string lain-proxy-pass)))))
  (with-eval-after-load 'url-http
    (defun url-https-proxy-connect (connection)
      (setq url-http-after-change-function 'url-https-proxy-after-change-function)
      (process-send-string connection
			   (format (concat "CONNECT %s:%d HTTP/1.1\r\n"
					   "Host: %s\r\n"
					   (let ((proxy-auth (let ((url-basic-auth-storage
								    'url-http-proxy-basic-auth-storage))
							       (url-get-authentication url-http-proxy nil 'any nil))))
					     (if proxy-auth (concat "Proxy-Authorization: " proxy-auth "\r\n")))
					   "\r\n")
				   (url-host url-current-object)
				   (or (url-port url-current-object)
				       url-https-default-port)
				   (url-host url-current-object))))))

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
