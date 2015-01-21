; my-packages.el
(defvar required-packages
  '(ace-isearch
    auto-complete
    coffee-mode
    color-theme
    company
    company-tern
    ecb
    evil
    exec-path-from-shell
    expand-region
    floobits
    flx
    flx-ido
    flycheck
    gnuplot
    helm
    helm-ag
    helm-css-scss
    helm-projectile
    js-doc
    js2-mode
    js2-refactor
    key-chord
    less-css-mode
    linum-relative
    magit
    magit-filenotify
    multiple-cursors
    org
    ov
    popup
    popwin
    powerline
    project-explorer
    projectile
    rainbow-delimiters
    smartparens
    smooth-scrolling
    tangotango-theme
    tern
    tern-auto-complete
    undo-tree
    web-beautify
    wgrep-ag))

; my-packages.el
(require 'cl)

; method to check if all packages are installed
(defun packages-installed-p ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

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
