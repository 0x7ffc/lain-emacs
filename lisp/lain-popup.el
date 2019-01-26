;; lain-popup.el -*- lexical-binding: t; -*-

;; Thanks to these document and code:
;;
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Choosing-Window.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Frame-Layouts-with-Side-Windows.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Window-Parameters.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Displaying-Buffers-in-Side-Windows.html
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/window.el
;; https://github.com/hlissner/doom-emacs/blob/develop/modules/ui/popup/autoload/popup.el

;; The idea is to manipulate `display-buffer-alist'
;;
;; (lain/set-popup-rules ;; '("^\\*[Hh]elp" :slot 0 :size 0.35 :select t))
;;    ||
;;    || will'be translated to this
;;    ||
;; (setq
;;  display-buffer-alist
;;  '(("^\\*[Hh]elp"
;;     (lain/popup-main)
;;     (actions . (display-buffer-reuse-window
;;		display-buffer-in-side-window))
;;     (side . bottom)
;;     (window-width . 0.5)
;;     (window-height . 0.35)
;;     (slot . 1)
;;     (reusable-frames . visible)
;;     (window-parameters
;;      (select . t)
;;      (quit . t)
;;      (popup . t)
;;      (mode-line-format . none)
;;      (no-other-window . t)))))

;;;###autoload
(defvar lain-popup-alist-default
  (list :actions '(display-buffer-reuse-window display-buffer-in-side-window)
	:side 'bottom
	:size 0.25
	:slot 0
	:reusable-frames 'visible))

;;;###autoload
(defvar lain-popup-params-defalut
  (list :popup t
	:quit t
	:select nil
	:modeline 'none
	:no-other-window t))

;;;###autoload
(defun lain/popup-windows ()
  (-filter 'lain/popup-window-p (window-list)))

;;;###autoload
(defun lain/popup-window-p (&optional window)
  (let ((window (or window (selected-window))))
    (and window
	 (windowp window)
	 (window-live-p window)
	 (window-parameter window 'popup))))

;;;###autoload
(defun lain/popup-close (&optional window force-p)
  (interactive (list (selected-window) current-prefix-arg))
  (let ((window (or window (selected-window))))
    (when (and (lain/popup-window-p window)
	       (or force-p
		   (window-parameter window 'quit)))
      (delete-window window))))

;;;###autoload
(defun lain/popup-close-all (&optional force-p)
  (interactive "P")
  (--each (lain/popup-windows)
    (when (or force-p
	      (window-parameter it 'quit))
      (delete-window it))))

;;;###autoload
(defun lain/popup-close-on-escape ()
  (if (lain/popup-window-p)
      (lain/popup-close)
    (lain/popup-close-all)))

;;;###autoload
(defun lain/popup-raise (window)
  (interactive (list (selected-window)))
  (unless (lain/popup-window-p window)
    (user-error "Can't raise a non-popup window"))
  (lain/popup-close window t)
  (display-buffer-pop-up-window (current-buffer) nil))

;;;###autoload
(defun lain/popup-main (buffer &optional alist)
  (interactive)
  (-let* ((origin (selected-window))
	  ((&alist 'actions) alist)
	  (popup (cl-loop for func in actions
			  if (funcall func buffer alist)
			  return it)))
    (set-window-dedicated-p popup 'popup)
    (let ((select (window-parameter popup 'select)))
      (if (functionp select)
	  (funcall select popup origin)
	(select-window (if select popup origin))))
    popup))

;;;###autoload
(defun lain/popup-make-rule (rule)
  (-let* (((pred . plist) rule)
	  (alist-keys
	   (-filter 'keywordp lain-popup-alist-default))
	  (params-keys
	   (-filter 'keywordp lain-popup-params-defalut))
	  (choose
	   (lambda (key default)
	     (if (plist-member plist key)
		 (plist-get plist key)
	       (plist-get default key))))
	  (alist
	   (--map (cons (intern (s-chop-prefix ":" (symbol-name it)))
			(funcall choose it lain-popup-alist-default))
		  (remove :size alist-keys)))
	  (alist
	   (cons (cons (if (memq (alist-get 'side alist) '(left right))
			   'window-width
			 'window-height)
		       (funcall choose :size lain-popup-alist-default))
		 alist))
	  (params
	   (--map (cons (intern (s-chop-prefix ":" (symbol-name it)))
			(funcall choose it lain-popup-params-defalut))
		  (remove :modeline params-keys)))
	  (params
	   (cons (cons 'mode-line-format
		       (if (plist-member plist :modeline)
			   (if (plist-get plist :modeline)
			       nil
			     'none)
			 (plist-get lain-popup-params-defalut :modeline)))
		 params)))
    `(,pred (lain/popup-main)
	    ,@alist
	    (window-parameters ,@params))))

;;;###autoload
(defun lain/set-popup-rules (&rest rules)
  (declare (indent 0))
  (setq display-buffer-alist
	(let ((-compare-fn (-lambda (a b) (equal (car a) (car b)))))
	  (-union display-buffer-alist
		  (-map 'lain/popup-make-rule rules)))))


;; Defalut rules
;; options:
;;   modeline: t/nil, default nil
;;   select: t/nil/function, default nil
;;   quit: t/nil, default t
;;   side: top/bottom/left/right, default bottom
;;   size: number/function, default 0.25

(lain/set-popup-rules
  '("^\\*[Hh]elp" :size 0.35 :select t)
  '("^\\*info\\*$" :size 0.45 :select t)
  '("^\\*Warning" :size 0.35)
  '("^\\*Backtrace" :size 0.35 :quit nil)
  '("^ \\*undo-tree\\*" :slot 2 :side right :select t)
  '("^\\*Pp Eval" :size shrink-window-if-larger-than-buffer :select ignore)
  '("^\\* Regexp Explain \\*$" :size shrink-window-if-larger-than-buffer)
  '("^\\*xref\\*$" :size 0.2 :select ignore)
  '("^\\*Async Shell Command\\*" :size 0.5 :select t))

(add-hook 'after-init-hook
	  (lambda () (add-hook 'lain-escape-hook #'lain/popup-close-on-escape t)))

(lain-leader-def
  "P"  '(:ignore t :wk "Popup")
  "Pd" 'lain/popup-close-all
  "Pr" 'lain/popup-raise)


;; Hacks

;; Info
(defun lain/popup-info-hack (&rest _)
  (when-let* ((win (get-buffer-window "*info*"))
	      (select (window-parameter win 'select))
	      (popup-p (lain/popup-window-p win)))
    (select-window win)))
(advice-add #'info-lookup-symbol :after #'lain/popup-info-hack)

(provide 'lain-popup)
