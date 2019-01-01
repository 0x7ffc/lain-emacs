;; lain-keybindings.el -*- lexical-binding: t; -*-


;; Dvorak keybindings core

(defmacro lain/dv-keys (states keymaps &rest args)
  "Swap `hjkl' with `htns'"
  (let ((args (--mapcat (list it (s-replace-all '(("j" . "t")
						  ("J" . "T")
						  ("k" . "n")
						  ("K" . "N")
						  ("l" . "s")
						  ("L" . "S")) it)) args)))
    `(general-swap-key ,states ,keymaps ,@args)))
(put 'lain/dv-keys 'lisp-indent-function 'defun)


;; Add keyword `:lain-major-mode' to use-package

(defmacro lain/create-major-mode-leader-map (mode &optional keymap)
  "Just like spacemacs's `spacemacs/set-leader-keys-for-major-mode'
It creates lain-<mode>-map to use with :general keyword"
  (let ((prefix-map (intern (format "lain-%s-map" mode)))
	(keymap (or keymap (intern (format "%s-map" mode)))))
    `(general-define-key
      :states 'normal
      :prefix ,lain-major-mode-leader-key
      :keymaps ',keymap
      :prefix-map ',prefix-map)))

(after use-package-core

  (defun use-package-normalize-lain-major-mode (name label arg &optional recursed)
    (cond
     ((not arg)
      (list (use-package-as-mode name)))
     ((use-package-non-nil-symbolp arg)
      (list arg))
     ((consp arg)
      (list arg))
     ((and (not recursed) (listp arg) (listp (cdr arg)))
      (mapcar #'(lambda (x) (car (use-package-normalize-lain-major-mode
			     name label x t))) arg))
     (t
      (use-package-error
       (concat label " wants symbol,"
	       "(symbol symbol) or list of these")))))

  (defun use-package-normalize/:lain-major-mode (name keyword args)
    (use-package-as-one (symbol-name keyword) args
      (apply-partially #'use-package-normalize-lain-major-mode name) t))

  (defun use-package-handler/:lain-major-mode (name _keyword arg rest state)
    (let ((body (use-package-process-keywords name rest state)))
      (use-package-concat
       (mapcar #'(lambda (var)
		   (if (consp var)
		       `(lain/create-major-mode-leader-map ,(car var) ,(cdr var))
		     `(lain/create-major-mode-leader-map ,var)))
	       arg)
       body)))

  (add-to-list 'use-package-keywords :lain-major-mode t)
  )


;; Main keybindings
;; mostly are bindings without corresponding packages

(general-def insert global
  [remap newline] 'newline-and-indent)

(lain-leader-def
  "TAB"   'lain/other-buffer
  "a"     '(:ignore t :wk "application")
  "b C-d" 'lain/kill-other-buffers
  "b"     '(:ignore t :wk "buffer")
  "bD"    'kill-buffer
  "bR"    'lain/rename-current-file
  "bY"    'lain/copy-whole-buffer-to-clipboard
  "bd"    'kill-current-buffer
  "bm"    'lain/switch-to-message-buffer
  "bn"    'previous-buffer
  "bs"    'lain/switch-to-scratch-buffer
  "bt"    'next-buffer
  "bx"    'kill-buffer-and-window
  "e"     '(:ignore t :wk "emacs/elisp")
  "eb"    'esup
  "ei"    'lain/find-init-file
  "eI"    'lain/find-lain-main-file
  "eq"    'save-buffers-kill-terminal
  "eu"    'lain/update
  "f"     '(:ignore t :wk "file")
  "fD"    'lain/delete-current-file
  "fR"    'lain/rename-current-file
  "fs"    'save-buffer
  "g"     '(:ignore t :wk "git")
  "h"     '(:ignore t :wk "help")
  "i"     '(:ignore t :wk "insert")
  "ib"    'lain/insert-page-break
  "in"    'lain/insert-newline-above
  "it"    'lain/insert-newline-below
  "m"     '(:ignore t :which-key "multiedit")
  "n"     '(:ignore t :wk "narrow")
  "nf"    'lain/narrow-to-defun
  "np"    'narrow-to-page
  "nr"    'narrow-to-region
  "nw"    'widen
  "p"     '(:ignore t :wk "project")
  "t"     '(:ignore t :wk "toggle/theme")
  "tF"    'toggle-frame-fullscreen
  "tC"    'lain/cycle-themes
  "u"     'universal-argument
  "w"     '(:ignore t :wk "window")
  "w1"    'delete-other-windows
  "w2"    'lain/split-window-below-and-focus
  "w3"    'lain/split-window-right-and-focus
  "wO"    'delete-other-windows
  "wd"    'delete-window
  "wo"    'other-window
  "ws"    'lain/swap-window
  "x"     '(:ignore t :wk "text")
  "xl"    '(:ignore t :wk "lines")
  "xlj"   'lain/join-lines
  "xls"   'lain/split-lines
  "x TAB" 'indent-rigidly
  )

(provide 'lain-keybindings)
