; my-packages.el
(defvar required-packages
  '(color-theme company-tern company dash-functional ecb flx-ido flx gnuplot helm-projectile helm async js2-mode org projectile pkg-info epl f dash s tern-auto-complete auto-complete popup tern undo-tree web-beautify multiple-cursors magit magit-filenotify less-css-mode helm-css-scss helm-ag coffee-mode js-doc js2-refactor expand-region evil key-chord smartparens linum-relative floobits ace-isearch popwin ov) "a list of packages to ensure are installed at launch.")

; my-packages.el
(require 'cl)

; method to check if all packages are installed
(defun packages-installed-p ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

; if not all packages are installed, check one by one and install the missing ones.
(unless (packages-installed-p)
; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
; install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
            (package-install p))))
