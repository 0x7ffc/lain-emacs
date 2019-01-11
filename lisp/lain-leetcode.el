;; lain-leetcode.el -*- lexical-binding: t; -*-

(use-package transient
  :straight (:host github :repo "magit/transient")
  :init
  (setq transient-history-file
	(no-littering-expand-var-file-name "transient-history.el")))

(require 'transient)
(require 'dash)
(require 'f)

(defvar lc-executable "leetcode-cli")

(defvar lc-user-name nil)

(defvar lc-password (getenv "LEETCODE_PASS"))

(defvar lc-lang "c"
  "Available langs:
bash c cpp csharp golang java javascript
mysql python python3 ruby scala swift")

(defvar lc-dir (f-canonical "~/.lc/emacs-leetcode/"))

(defvar lc-submission-dir (f-canonical "~/.lc/emacs-leetcode/submission"))

(defvar lc-buffer "*leetcode*")

;;;###autoload
(defun lc-popup (&rest command)
  (let ((async-shell-command-buffer 'rename-buffer))
    (async-shell-command (lc-normalize-args command))))

;;;###autoload
(defun lc-normalize-args (&rest args)
  (s-join " " (-flatten (cons lc-executable args))))

;;;###autoload
(defun lc-run (&rest args)
  (let ((command (lc-normalize-args args)))
    (message "Leetcode running: %s" command)
    (prog1
	(shell-command-to-string command)
      (message "Done!"))))

(define-transient-command lc-help-popup ()
  ["Actions"
   [("u" "User" lc-user-popup)
    ("v" "Version" lc-version-popup)
    ("l" "List" lc-list-popup)]
   [("s" "Show" lc-show-popup)
    ("f" "Files" lc-files)
    ("p" "Sumbit" lc-submit)]
   [("t" "Test" lc-test-popup)
    ("x" "Submission" lc-submission-popup)]
   ])
(defalias 'leetcode 'lc-help-popup)


;; version
(define-transient-command lc-version-popup ()
  ["Arguments" ("-v" "verbose" "-v")]
  ["Actions" [("v" "Show version info" lc-version)]])

;;;###autoload
(defun lc-version (&optional args)
  (interactive (list (transient-args)))
  (lc-popup "version" args))


;; user
;; TODO Need support, leetcode-cli has no login argument
(define-transient-command lc-user-popup ()
  ["Actions"
   [("l" "TODO Login" lc-user-login)
    ("o" "TODO Logout" lc-user-logout)
    ("s" "TODO Show currnt account" leetcod-user-show)]])


;; list
(define-transient-command lc-list-popup ()
  ["Arguments"
   [("-e" "easy" "e")
    ("-E" "not easy(m+h)" "E")
    ("-m" "medium" "m")
    ("-M" "not medium(e+h)" "M")
    ("-h" "hard" "h")
    ("-H" "not hard" "H")]
   [("-d" "Ac-ed" "d")
    ("-D" "not Ac-ed" "D")
    ("-l" "locked" "l")
    ("-L" "not locked" "L")
    ("-s" "started" "s")
    ("-S" "not started" "S")]
   [("-t" "filter by tag" "-t=" read-string)
    ("-k" "keyword" " " read-string)
    ("-T" "show statistic" "-s")]]
  ["Actions"
   [("l" "Query by conditions" lc-list)]])

;;;###autoload
(defun lc-list (&optional args)
  (interactive (list (transient-args)))
  (lc-popup "list" "-q"
	    (apply 's-concat (--take-while (not (s-starts-with? "-" it)) args))
	    (--drop-while (not (s-starts-with? "-" it)) args)))


;; show
(define-transient-command lc-show-popup ()
  ["Arguments"
   ("-g" "generate file" "-g")
   ("-x" "add description" "-x")
   ("-k" "id/name" " " read-string)
   ("-c" "only show template" "-c")]
  ["Actions"
   [("s" "Display question" lc-show)]])

;;;###autoload
(defun lc-show (&optional args)
  (interactive (list (transient-args)))
  (lc-ensure-dir)
  (let ((args (-cons* "-l" lc-lang
		      "-o" lc-dir
		      args)))
    (if (-contains-p args "-g")
	(-when-let* ((default-directory lc-dir)
		     (content (lc-run "show" args))
		     (file-name (-some->> content
					  (s-match "\\* Source Code:\\s-+.*")
					  (car)
					  (s-split ":")
					  (cadr)
					  (s-trim-left))))
	  (find-file file-name))
      (lc-popup "show" args))))


;; files
;;;###autoload
(defun lc-ensure-dir ()
  (unless (or (f-exists-p lc-dir)
	      (f-directory-p lc-dir))
    (make-directory lc-dir)))

;;;###autoload
(defun lc-files ()
  (interactive)
  (lc-ensure-dir)
  (let ((default-directory lc-dir))
    (counsel-find-file)))

;;;###autoload
(defmacro with-leetcode-file (file &rest body)
  (declare (indent defun))
  `(-if-let* ((buffer (current-buffer))
	      (,file (f-canonical (buffer-file-name buffer)))
	      (_ (f-child-of-p ,file lc-dir)))
       (progn ,@body)
     (user-error "Current file is not one of the files inside %s" lc-dir)))


;; submit
;;;###autoload
(defun lc-submit ()
  (interactive)
  (with-leetcode-file file
    (lc-run "submit" file)))


;; test
(define-transient-command lc-test-popup ()
  ["Arguments" ("-t" "test case" "-t=" read-string)]
  ["Actions" [("t" "test" lc-test)]])

;;;###autoload
(defun lc-test (&optinoal args)
  (interactive (list (transient-args)))
  (with-leetcode-file file
    (lc-run "test" file args)))


;; submission
(define-transient-command lc-submission-popup ()
  ["Arguments"
   ("-a" "all questions" "-a")
   ("-x" "with details" "-x")
   ("-k" "id/name" " " read-string)]
  ["Actions"
   ("x" "submission" lc-submission)])

;;;###autoload
(defun lc-submission (&optional args)
  (interactive (list (transient-args)))
  (let ((args (-cons* "-o" lc-submission-dir
		      "-l" lc-lang
		      args)))
    (lc-run "submission" args)))


(provide 'lain-leetcode)

(use-feature lain-leetcode
  :general
  (lain-leader-map
   "al" 'leetcode)
  :init
  (setq lc-user-name "gezimerlin@gmail.com"))
