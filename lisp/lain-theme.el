;; lain-theme.el -*- lexical-binding: t; -*-

(defvar lain-packages-with-themes
  '((zenburn-theme
     . (zenburn))
    (srcery-theme
     . (srcery))
    (leuven-theme
     . (leuven
	leuven-dark))
    (ample-theme
     . (ample-light
	ample-flat))
    (solarized-theme
     . (solarized-light
	solarized-dark))
    (tao-theme
     . (tao-yin
	tao-yang))
    (doom-themes
     . (doom-challenger-deep
	doom-city-lights
	doom-dracula
	doom-molokai
	doom-mono-dark
	doom-mono-light
	doom-nord
	doom-nord-light
	doom-nova
	doom-one
	doom-one-light
	doom-opera
	doom-opera-light
	doom-peacock
	doom-spacegrey
	doom-solarized-light
	doom-tomorrow-day
	doom-tomorrow-night
	doom-tron
	doom-vibrant
	doom-xbase16-3024))
    (base16-theme
     . (base16-3024
	base16-apathy
	base16-ashes
	base16-atelier-cave-light
	base16-atelier-cave
	base16-atelier-dune-light
	base16-atelier-dune
	base16-atelier-estuary-light
	base16-atelier-estuary
	base16-atelier-forest-light
	base16-atelier-forest
	base16-atelier-heath-light
	base16-atelier-heath
	base16-atelier-lakeside-light
	base16-atelier-lakeside
	base16-atelier-plateau-light
	base16-atelier-plateau
	base16-atelier-savanna-light
	base16-atelier-savanna
	base16-atelier-seaside-light
	base16-atelier-seaside
	base16-atelier-sulphurpool-light
	base16-atelier-sulphurpool
	base16-bespin
	base16-brewer
	base16-bright
	base16-brushtrees
	base16-brushtrees-dark
	base16-chalk
	base16-circus
	base16-classic-dark
	base16-classic-light
	base16-codeschool
	base16-cupcake
	base16-cupertino
	base16-darktooth
	base16-default-dark
	base16-default-light
	base16-dracula
	base16-eighties
	base16-embers
	base16-flat
	base16-github
	base16-google-dark
	base16-google-light
	base16-grayscale-dark
	base16-grayscale-light
	base16-greenscreen
	base16-gruvbox-dark-hard
	base16-gruvbox-dark-medium
	base16-gruvbox-dark-pale
	base16-gruvbox-dark-soft
	base16-gruvbox-light-hard
	base16-gruvbox-light-medium
	base16-gruvbox-light-soft
	base16-harmonic-dark
	base16-harmonic-light
	base16-hopscotch
	base16-irblack
	base16-isotope
	base16-london-tube
	base16-macintosh
	base16-marrakesh
	base16-materia
	base16-material-darker
	base16-material-lighter
	base16-material-palenight
	base16-material
	base16-mellow-purple
	base16-mexico-light
	base16-mocha
	base16-monokai
	base16-nord
	base16-ocean
	base16-oceanicnext
	base16-onedark
	base16-one-light
	base16-paraiso
	base16-phd
	base16-pico
	base16-pop
	base16-porple
	base16-railscasts
	base16-rebecca
	base16-seti
	base16-seti-ui
	base16-shapeshifter
	base16-solarflare
	base16-solarized-dark
	base16-solarized-light
	base16-spacemacs
	base16-summerfruit-dark
	base16-tomorrow
	base16-tube
	base16-twilight
	base16-unikitty-dark
	base16-unikitty-light
	base16-woodland
	base16-xcode-dusk
	base16-zenburn)))
  "alist of packages and its themes")

(defvar lain-current-theme nil
  "Internal variable storing currently loaded theme.")

(defun lain/load-theme (name)
  "Load theme with its corresponding package using `use-package'"
  (let ((pkg (car (--first
		   (-contains? (cdr it) name)
		   lain-packages-with-themes))))
    (setq-default lain-current-theme name)
    (if (null pkg)
	(message "There in no theme named: %s" name)
      (eval `(use-package ,pkg
	       :demand t
	       :config
	       (load-theme ',name t))))))

(lain/load-theme (car lain-themes))

;;;###autoload
(defun lain/cycle-themes (&optional backward)
  "Cycle through themes defined in `lain-themes.'
When BACKWARD is non-nil, cycle backwards."
  (interactive "P")
  (let* ((themes (if backward (reverse lain-themes) lain-themes))
	 (next-theme (car (or (cdr (memq lain-current-theme themes))
			      themes))))
    (disable-theme lain-current-theme)
    (lain/load-theme next-theme)
    (message "Load Theme: %s" next-theme)))

(provide 'lain-theme)
