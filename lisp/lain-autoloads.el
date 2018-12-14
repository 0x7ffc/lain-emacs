;; lain-autoloads.el -*- lexical-binding: t; -*-

;;;###autoload
(defun lain/update ()
  (interactive)
  (when (y-or-n-p "Update now?")
    (straight-pull-all)
    (straight-prune-build)
    (straight-rebuild-all)))

;;;###autoload
(defun lain/save-file ()
  "Save buffer if file exists, also silent minibuffer echo"
  (interactive)
  (when buffer-file-name
    (let ((inhibit-message t))
      (save-buffer))))

;;;###autoload
(defun lain/other-buffer ()
  "Switch back and forth between current and last buffer in current window"
  (interactive)
  ;; when second argument is nil, default to call `other-buffer'
  (switch-to-buffer nil nil t))

;;;###autoload
(defun lain/copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max))
  (message "Buffer copied to clipboard!"))

;;;###autoload
(defun lain/kill-other-buffers (&optional arg)
  "Kill all other buffers.
If the universal prefix argument then don't kill specail buffer."
  (interactive "P")
  (when (y-or-n-p (format "Killing all buffers except \"%s\"? "
			  (buffer-name)))
    (dolist (buffer (buffer-list) nil)
      (unless (or (equal (current-buffer) buffer)
		  (or (equal '(4) arg)
		      (buffer-file-name buffer)))
	(kill-buffer buffer))
      (delete-other-windows)
      (message "Buffers deleted!"))))

;;;###autoload
(defun lain/swap-window ()
  (interactive)
  (let ((curr-win-buffer (window-buffer))
	(next-win-buffer (window-buffer (next-window))))
    (set-window-buffer (selected-window) next-win-buffer)
    (set-window-buffer (next-window) curr-win-buffer)
    (select-window (next-window))))

;;;###autoload
(defun lain/split-window-below-and-focus ()
  (interactive)
  (select-window (split-window-below)))

;;;###autoload
(defun lain/split-window-right-and-focus ()
  (interactive)
  (select-window (split-window-right)))

;;;###autoload
(defun lain/find-init-file ()
  (interactive)
  (find-file user-init-file))

;;;###autoload
(defun lain/switch-to-scratch-buffer ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*")))

;;;###autoload
(defun lain/switch-to-message-buffer ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*Messages*")))

;;;###autoload
(defun lain/delete-current-file ()
  "Deleted current file and its buffer"
  (interactive)
  (let ((file (buffer-file-name)))
    (when (and file
	       (file-exists-p file)
	       (y-or-n-p (format "Are you sure you want to delete this file: %s" file)))
      (delete-file file t)
      (message "Deleted file: %s" file))
    (kill-buffer)))

;;;###autoload
(defun lain/rename-current-file ()
  "Rename current buffer and its visiting file"
  (interactive)
  (let ((file (buffer-file-name)))
    (when (and file (file-exists-p file))
      (let ((new-name (read-file-name "New name: " (file-name-directory file))))
	(dired-rename-file file new-name nil)
	(when (and (featurep 'projectile)
		   (projectile-project-p))
	  (call-interactively #'projectile-invalidate-cache))
	(message "Successfully renamed %s to %s" file new-name)))))

;;;###autoload
(defun lain/insert-newline-above ()
  "Insert a newline above current line, and don't move cursor"
  (interactive)
  (save-excursion
    (move-beginning-of-line 1)
    (newline)))

;;;###autoload
(defun lain/insert-newline-below ()
  "Insert a newline below current line, and don't move cursor"
  (interactive)
  (save-excursion
    (move-end-of-line 1)
    (newline)))

;;;###autoload
(defun lain/insert-page-break ()
  "Insert a page break `\^L'"
  (interactive)
  (insert "\C-l"))

;;;###autoload
(defun lain/join-lines (beg end)
  "Join items on separate lines to a list by some separator
Default separator is comma
If no region is selected then works on curren buffer "
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (let* ((lines (s-lines (buffer-substring-no-properties beg end)))
	 (insertion (s-join (read-string "Enter separator: " ",") lines)))
    (delete-region beg end)
    (insert insertion)))

;;;###autoload
(defun lain/split-lines (beg end)
  "Split expression into multiple lines by some separator
Default separator is comma
If no region is selected then works on current line"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2))))
  (let* ((sep (read-string "Enter separator: " ","))
	 (lines (s-split sep (buffer-substring-no-properties beg end)))
	 (insertion (s-join "\n" lines)))
    (delete-region beg end)
    (insert insertion)))

;;;###autoload
(defun lain/symbol-at-point ()
  "Return symbol at point or selected region when region is active"
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'symbol t)))

(provide 'lain-autoloads)
