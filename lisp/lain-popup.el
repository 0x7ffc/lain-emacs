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
	:width 40
	:height 0.25
	:slot 0
	:reusable-frames 'visible
	:popup t
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
(defun lain/popup-main (buffer &optional alist)
  (interactive)
  (-let* ((origin (selected-window))
	  ((&alist 'actions) alist)
	  (popup (cl-loop for func in actions
			  if (funcall func buffer alist)
			  return it)))
    (set-window-dedicated-p popup 'popup)
    (select-window (if (window-parameter popup 'select) popup origin))
    popup))

;;;###autoload
(defun lain/make-rule (rule)
  (-let* (((pred . plist) rule)
	  ((&plist :actions :side :height :width :slot
		   :quit :select :modeline) plist)
	  ((&plist :actions daction :side dside
		   :height dheight :width dwidth
		   :slot dslot :reusable-frames dreusable-frames
		   :quit dquit :select dselect :popup dpopup
		   :modeline dmodeline :no-other-window dno-other-window) lain-popup-alist-default)
	  (alist
	   `((actions . ,(or actions daction))
	     (side . ,(or side dside))
	     (window-height . ,(or height dheight))
	     (window-width . ,(or width dwidth))
	     (slot . ,(or slot dslot))
	     (reusable-frames . ,dreusable-frames)))
	  (params
	   `((quit . ,(or quit dquit))
	     (select . ,(or select dselect))
	     (popup . ,dpopup)
	     (mode-line-format . ,(or modeline dmodeline))
	     (no-other-window . ,dno-other-window))))
    `(,pred (lain/popup-main)
	    ,@alist
	    (window-parameters ,@params))))

;;;###autoload
(defun lain/set-popup-rules (&rest rules)
  (declare (indent 0))
  (setq display-buffer-alist
	(let ((-compare-fn (-lambda (a b) (equal (car a) (car b)))))
	  (-union display-buffer-alist
		  (-map 'lain/make-rule rules)))))

(lain/set-popup-rules
  '("^\\*[Hh]elp" :slot 0 :size 0.35 :select t))

(add-hook 'after-init-hook
	  (lambda () (add-hook 'lain-escape-hook #'lain/popup-close-on-escape t)))

(provide 'lain-popup)
