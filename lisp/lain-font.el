;; lain-font.el -*- lexical-binding: t; -*-


;; Set default font using `lain-font'
(defun lain/set-default-font (font)
  (when (find-font (font-spec :name (car font)))
    (let* ((font-name (car font))
	   (props (cdr font))
	   (fontspec (apply 'font-spec :name font-name props)))
      (add-to-list 'default-frame-alist
		   (cons 'font (font-xlfd-name fontspec))))))
(lain/set-default-font lain-font)


;; Prettify symbols
;; Support fonts:
;;   - Hasklig
;;   - Fira Code
(defconst lain-current-font-name
  (downcase (car lain-font)))

(defconst lain-ligatures-startpoint #Xe100)

(defun lain/correct-symbol-bounds (ligature-alist)
  "Prepend non-breaking spaces to a ligature.
This way `compose-region' (called by `prettify-symbols-mode') will use the
correct width of the symbols instead of the width measured by `char-width'."
  (-map (-lambda ((key . code)) (cons key (string ?\t code))) ligature-alist))

(defun lain/ligature-list (ligatures start)
  "(a b c) -> ((a start) (b (+1 start)) (c (+2 start)))"
  (let ((codepoints (-iterate '1+ start (length ligatures))))
    (-zip ligatures codepoints)))

(when (s-equals? "hasklig" lain-current-font-name)
  (defconst lain-hasklig-ligatures
    '("&&" "***" "*>" "\\\\" "||" "|>" "::"
      "==" "===" "==>" "=>" "=<<" "!!" ">>"
      ">>=" ">>>" ">>-" ">-" "->" "-<" "-<<"
      "<*" "<*>" "<|" "<|>" "<$>" "<>" "<-"
      "<<" "<<<" "<+>" ".." "..." "++" "+++"
      "/=" ":::" ">=>" "->>" "<=>" "<=<" "<->"))
  (defconst lain-current-ligature-alist
    (lain/correct-symbol-bounds
     (lain/ligature-list lain-hasklig-ligatures lain-ligatures-startpoint))))

(when (s-equals? "fira code" lain-current-font-name)
  (defconst lain-fira-code-ligatures
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
  (defconst lain-current-ligature-alist
    (lain/correct-symbol-bounds
     (lain/ligature-list lain-fira-code-ligatures lain-ligatures-startpoint))))

(when (bound-and-true-p lain-use-ligature)
  (defun lain/setup-ligatures ()
    (setq-local prettify-symbols-alist
		(-union prettify-symbols-alist lain-current-ligature-alist))
    (setq-local prettify-symbols-unprettify-at-point 'right-edge)
    (prettify-symbols-mode +1))

  (general-add-hook '(prog-mode-hook text-mode-hook) 'lain/setup-ligatures))

(provide 'lain-font)
