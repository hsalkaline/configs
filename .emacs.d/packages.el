; my-packages.el
(defvar required-packages
  '(color-theme
    company-tern
    company
    dash-functional
    ecb
    flx-ido
    flx
    gnuplot
    helm-projectile
    helm
    async
    js2-mode
    org
    projectile
    pkg-info
    epl
    f
    dash
    s
    tern-auto-complete
    auto-complete
    popup
    tern
    undo-tree
    web-beautify
    multiple-cursors
    magit
    magit-filenotify
    less-css-mode
    helm-css-scss
    helm-ag
    coffee-mode
    js-doc
    js2-refactor
    expand-region
    evil
    key-chord
    smartparens
    linum-relative
    floobits
    ace-isearch
    popwin
    ov
    flycheck
    wgrep-ag))

; my-packages.el
(require 'cl)

; method to check if all packages are installed
(defun packages-installed-p ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (packages-installed-p)
  (message "%s" "There are not installed packages!"))

(defun alkaline/install-missing-packages ()
  "Checking for missing packages and installs them"
  (interactive)
  (unless (packages-installed-p)
    (message "%s" "Emacs is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    (dolist (p required-packages)
      (when (not (package-installed-p p))
        (package-install p)))))

(provide 'alkaline/install-missing-packages)
