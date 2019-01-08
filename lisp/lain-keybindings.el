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


(defmacro lain/set-major-mode-leader-keys (mode &rest bindings)
  "Just like spacemacs's `spacemacs/set-leader-keys-for-major-mode'
It creates lain-<mode>-map to use with :general keyword"
  (let ((prefix-map (intern (format "lain-%s-map" mode)))
	(keymap (intern (format "%s-map" mode))))
    `(general-define-key
      :states 'normal
      :prefix ,lain-major-mode-leader-key
      :keymaps ',keymap
      :prefix-map ',prefix-map
      ,@bindings)))
(put 'lain/set-major-mode-leader-keys 'lisp-indent-function 'defun)


;; Main keybindings
;; mostly are bindings without corresponding packages

(defvar lain-escape-hook nil)

;;;###autoload
(defun lain/escape ()
  (interactive)
  (cond ((minibuffer-window-active-p (minibuffer-window))
	 (abort-recursive-edit))
	((cl-find-if #'funcall lain-escape-hook))
	((keyboard-quit))))

(general-def
  [remap newline]       'newline-and-indent
  [remap keyboard-quit] 'lain/escape
  "<escape>"            'lain/escape)

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
  "bs"    'save-buffer
  "bS"    'lain/switch-to-scratch-buffer
  "bt"    'next-buffer
  "bx"    'kill-buffer-and-window
  "e"     '(:ignore t :wk "emacs/elisp")
  "eI"    'lain/find-lain-main-file
  "eb"    'esup
  "ei"    'lain/find-init-file
  "eq"    'save-buffers-kill-terminal
  "eu"    'lain/update
  "f"     '(:ignore t :wk "file")
  "fD"    'lain/delete-current-file
  "fR"    'lain/rename-current-file
  "fS"    'evil-write-all
  "fE"    'lain/sudo-this-file
  "f C-e" 'lain/sudo-find-file
  "fl"    'find-file-literally
  "g"     '(:ignore t :wk "git")
  "h"     '(:ignore t :wk "help")
  "i"     '(:ignore t :wk "insert")
  "ib"    'lain/insert-page-break
  "in"    'lain/insert-newline-above
  "it"    'lain/insert-newline-below
  "j"     '(:ignore t :wk "jump")
  "m"     '(:ignore t :wk "multiedit")
  "n"     '(:ignore t :wk "narrow")
  "nf"    'lain/narrow-to-defun
  "np"    'narrow-to-page
  "nr"    'narrow-to-region
  "nw"    'widen
  "p"     '(:ignore t :wk "project")
  "t"     '(:ignore t :wk "toggle/theme")
  "tC"    'lain/cycle-themes
  "tF"    'toggle-frame-fullscreen
  "tc"    'subword-mode
  "u"     'universal-argument
  "w"     '(:ignore t :wk "window")
  "w1"    'delete-other-windows
  "w2"    'lain/split-window-below-and-focus
  "w3"    'lain/split-window-right-and-focus
  "wO"    'delete-other-windows
  "wd"    'delete-window
  "wo"    'other-window
  "ws"    'lain/swap-window
  "x TAB" 'indent-rigidly
  "x"     '(:ignore t :wk "text")
  "xl"    '(:ignore t :wk "lines")
  "xlj"   'lain/join-lines
  "xls"   'lain/split-lines
  "xld"   'delete-matching-lines
  )

(provide 'lain-keybindings)
