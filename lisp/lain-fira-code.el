;; lain-fira-code.el -*- lexical-binding: t; -*-

;;;###autoload
(defun lain-fira-code-mode--make-alist (list)
  "Generate prettify-symbols alist from LIST."
  (let ((idx -1))
    (mapcar
     (lambda (s)
       (setq idx (1+ idx))
       (let* ((code (+ #Xe100 idx))
	  (width (string-width s))
	  (prefix ())
	  (suffix '(?\s (Br . Br)))
	  (n 1))
     (while (< n width)
       (setq prefix (append prefix '(?\s (Br . Bl))))
       (setq n (1+ n)))
     (cons s (append prefix suffix (list (decode-char 'ucs code))))))
     list)))

;;;###autoload
(defconst lain-fira-code-mode--ligatures
  '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
    "{-" "[]" "::" ":::" ":=" "!!" "!=" "!==" "-}"
    "--" "---" "-->" "->" "->>" "-<" "-<<" "-~"
    "#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_("
    ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*"
    "/**" "/=" "/==" "/>" "//" "///" "&&" "||" "||="
    "|=" "|>" "^=" "$>" "++" "+++" "+>" "=:=" "=="
    "===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-" ">="
    ">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>"
    "<$" "<$>" "<!--" "<-" "<--" "<->" "<+" "<+>" "<="
    "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~"
    "<~~" "</" "</>" "~@" "~-" "~=" "~>" "~~" "~~>" "%%"))

;;;###autoload
(defvar lain-fira-code-mode--old-prettify-alist)

;;;###autoload
(defun lain-fira-code-mode--enable ()
  "Enable Fira Code ligatures in current buffer."
  (setq-local lain-fira-code-mode--old-prettify-alist prettify-symbols-alist)
  (setq-local prettify-symbols-alist
	      (append (lain-fira-code-mode--make-alist lain-fira-code-mode--ligatures)
		      lain-fira-code-mode--old-prettify-alist))
  (prettify-symbols-mode t))

;;;###autoload
(defun lain-fira-code-mode--disable ()
  "Disable Fira Code ligatures in current buffer."
  (setq-local prettify-symbols-alist lain-fira-code-mode--old-prettify-alist)
  (prettify-symbols-mode -1))

;;;###autoload
(define-minor-mode lain-fira-code-mode
  "Fira Code ligatures minor mode"
  :lighter " Fira Code"
  (setq-local prettify-symbols-unprettify-at-point 'right-edge)
  (if lain-fira-code-mode
      (lain-fira-code-mode--enable)
    (lain-fira-code-mode--disable)))

;;;###autoload
(defun lain-fira-code-mode--setup ()
  "Setup Fira Code Symbols"
  (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol"))

(provide 'lain-fira-code)
