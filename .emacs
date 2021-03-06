; melpa packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(when (not (require 'use-package nil 'noerror))
  (package-refresh-contents)
  (package-install 'use-package)
  (require 'use-package))

(require 'use-package)

;;debug
;; (toggle-debug-on-error)

;; PATH for graphics emacs
(use-package exec-path-from-shell
  :ensure t
  :if window-system
  :init
  (exec-path-from-shell-initialize))

;; no menu
(menu-bar-mode -1)
(tool-bar-mode -1)

;; lang/coding
(set-language-environment 'Russian)
(prefer-coding-system 'utf-8)

;; yes/no -> y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;;no lockfiles
(setq create-lockfiles nil)

;;ls --dired
(setq dired-use-ls-dired nil)
(require 'dired-x)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

; line numbers
(setq linum-format "%s ")
(add-hook 'prog-mode-hook 'linum-mode)

; auto-save
(setq auto-save-visited-file-name t)

;parens
(show-paren-mode t)

;recentf-mode
(recentf-mode t)

;restore session
;; (desktop-save-mode t)

;; trail whitespace and convert tabs to spaces on save
(add-hook 'before-save-hook (lambda() (if (and (eq nil (string-match "makefile" (symbol-name (buffer-local-value 'major-mode (current-buffer)))))
                                                  (eq nil (string-match "perl-mode" (symbol-name (buffer-local-value 'major-mode (current-buffer)))))
                                                  (eq nil (string-match "markdown" (symbol-name (buffer-local-value 'major-mode (current-buffer))))))
                                        (alkaline/cleanup-buffer-safe))))
;; (add-hook 'before-save-hook (lambda() (unless (string-match "makefile" (symbol-name (buffer-local-value 'major-mode (current-buffer))))
                                        ;; (alkaline/cleanup-buffer-safe))))

(setq indent-tabs-mode nil)
(global-set-key (kbd "<f6>") 'quoted-insert)

;; eshell
(global-set-key (kbd "<f8>") 'eshell)

;backups in temp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

; replace in region
(delete-selection-mode t)

;;paste C-w
(global-set-key "\C-w" 'yank)

;; ido
;; (ido-mode t)

;; \C-c\C-w copy regexp
;; \C-c\C-q quit
;; normal escaping in re-builder
(require 're-builder)
(setq reb-re-syntax 'string)

(put 'narrow-to-region 'disabled nil)
;;tramp
(setq tramp-default-method "ssh")

;; screen
(when (not 'display-graphic-p)
  (defun terminal-init-screen ()
    "Terminal initialization function for screen-256color."
    (load "term/xterm")
    (xterm-register-default-colors)
    (tty-set-up-initial-frame-faces))
  (terminal-init-screen))

;;colors
(use-package flatland-theme
  :ensure t
  :config
  (load-theme 'flatland t))
;; (use-package monokai-theme
;; (use-package tangotango-theme
;;   :ensure t
;;   :config
;;   (load-theme 'tangotango t))
;; (use-package monokai-theme
;;   :ensure t
;;   :config
;;   (load-theme 'monokai t))
;; (use-package leuven-theme
;;   :ensure t
;;   :config
;;   (load-theme 'leuven t))
;; ;; ;; key-chord
;; (use-package key-chord
;;   :config
;;   (progn
;;     (key-chord-mode t)
;;     (key-chord-define-global "yy" 'evil-yank-line))
;;   :bind
;;   ("M-k". key-chord-mode))

;; evil-mode
(use-package evil
  :ensure t
  :bind(("C-v" . set-mark-command)
        ("C-l" . evil-forward-char)
        ("C-h" . evil-backward-char)
        ("C-k" . evil-previous-line)
        ("C-j" . evil-next-line)
        ("M-j" . evil-join)))

(use-package dot-mode
  :ensure t
  :init
  (add-hook 'find-file-hooks 'dot-mode-on)
  :config
  (define-key dot-mode-map (kbd "M-'") 'dot-mode-execute))

;;jumping
(require 'cl)
(use-package ov
  :ensure t)
(use-package dash
  :ensure t)

;;selecting
(use-package expand-region
  :ensure t
  :bind ("C-q" . er/expand-region))

(use-package multiple-cursors
  :ensure t
  :bind(("M-q" . mc/mark-all-dwim)
        ("M-e" . mc/mark-more-like-this-extended)
        ("M-d" . mc/edit-lines)))

(use-package popwin
  :ensure t
  :config (popwin-mode t))

(electric-pair-mode t)
;;;(electric-layout-mode t)

(use-package company
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'company-mode))

(use-package undo-tree
  :ensure t
  :init
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-history-directory-alist `((".*" . "~/.emacs.d/undo")))
    (setq undo-tree-auto-save-history t)
    (defadvice undo-tree-undo (around keep-region activate)
      (if (use-region-p)
          (let ((m (set-marker (make-marker) (mark)))
                (p (set-marker (make-marker) (point))))
            ad-do-it
            (goto-char p)
            (set-mark m)
            (set-marker p nil)
            (set-marker m nil))
        ad-do-it))))

(use-package js2-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.bemhtml\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.bemhtml.js\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.xjst\\'" . js2-mode))
  :config
  (progn
    (define-key js2-mode-map (kbd "M-j") nil)
    (define-key js2-mode-map (kbd "TAB") (lambda()
                                           (interactive)
                                           (let ((yas/fallback-behavior 'return-nil))
                                             (unless (yas/expand)
                                               (indent-for-tab-command)
                                               (if (looking-back "^\s*")
                                                   (back-to-indentation))))))
    (use-package tern
      :ensure t
      :init
      (add-hook 'js2-mode-hook #'(lambda () (tern-mode t)))
      :config
      (progn
        (define-key tern-mode-keymap (kbd "\C-c\C-c") nil)
        (define-key tern-mode-keymap (kbd "\C-c\C-d") nil)
        (use-package company-tern
          :ensure t
          :config
          (add-to-list 'company-backends 'company-tern))))
    (use-package js-doc
      :ensure t
      :init
      (add-hook 'js2-mode-hook
                #'(lambda ()
                    (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc))))))

(use-package js2-refactor
      :ensure t
      :init
      (progn
        (require 'js2-refactor)
        (add-hook 'js2-mode-hook #'js2-refactor-mode)
        (js2r-add-keybindings-with-prefix "C-c C-r")))

(use-package helm
  :ensure t
  :init
  (require 'helm-config)
  :config
  (progn
    (setq helm-locate-fuzzy-match t
          helm-M-x-fuzzy-match t
          helm-buffers-fuzzy-matching t
          helm-recentf-fuzzy-match t
          helm-buffer-max-length 40)
    (helm-autoresize-mode t)
    (use-package helm-ag
      :ensure t
      :init
      (progn
        (setq helm-ag-always-set-extra-option t)
        (setq helm-ag-command-option "--ignore=test.bundles --ignore=desktop.bundles --ignore=node_modules")
        (setq helm-ag-fuzzy-match t)))

    (use-package helm-css-scss
      :ensure t)

    (use-package helm-swoop
      :ensure t))
  :bind (("M-x" . helm-M-x)
         ("M-a" . helm-buffers-list)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         ("M-y" . helm-show-kill-ring)
         ("C-x C-r" . helm-recentf)
         ("M-m" . helm-mark-ring)
         ("C-s" . helm-swoop)))

(use-package magit
  :ensure t
  :bind ("C-c m" . magit-status))
  ;; :config
  ;; (use-package magit-filenotify
  ;;   :ensure t
  ;;   :init
  ;;   (add-hook 'magit-status-mode-hook 'magit-filenotify-mode)))

;; (use-package flyspell
;;   :ensure t
;;   :init
;;   (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(use-package smooth-scrolling
  :ensure t)

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(use-package ace-jump-mode
  :ensure t
  :bind("C-f" . ace-jump-char-mode))

;; (use-package swiper-helm
;;   :ensure t
;;   :bind("C-s" . swiper-helm))

(use-package alkaline
  :load-path "alkaline")

(use-package floobits
  :ensure t)
(use-package flx-ido
  :ensure t)
(use-package flycheck
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'flycheck-mode))

(use-package gnuplot
  :ensure t)

(use-package less-css-mode
  :ensure t)

(use-package web-beautify
  :ensure t)

(use-package ag
  :ensure t
  :bind ("C-c f" . ag-project))

(use-package wgrep-ag
  :ensure t
  :init
  (progn
    (autoload 'wgrep-ag-setup "wgrep-ag")
    (add-hook 'ag-mode-hook 'wgrep-ag-setup))
  :config
  (progn
    (require 'dired)
    (define-key dired-mode-map (kbd "C-c C-p") 'wdired-change-to-wdired-mode)
    (setq wgrep-auto-save-buffer t)))

(use-package projectile
  :ensure t
  :init
  (progn
    (projectile-global-mode t)
    (setq projectile-completion-system 'helm)
    (setq projectile-switch-project-action 'projectile-dired)
    (setq projectile-enable-caching t)
    (add-to-list 'projectile-other-file-alist '("js" . ("spec.js" "test.js")) t)
    (add-to-list 'projectile-other-file-alist '("spec.js" . ("js")) t)
    (add-to-list 'projectile-other-file-alist '("test.js" . ("js")) t)
    (define-key projectile-command-map (kbd "o") 'projectile-find-other-file)
    (use-package helm-projectile
      :ensure t
      :config
      (helm-projectile-on)))
  :bind
  ("C-p" . helm-projectile))

(setq compilation-scroll-output t)
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(use-package persp-projectile
  :ensure t)

(use-package restclient
  :ensure t
  :config
  (progn
    (defun alkaline/restclient ()
      (interactive)
    (let ((buffer (generate-new-buffer "restclient")))
      (set-buffer buffer)
      (restclient-mode)
      (switch-to-buffer buffer)))
    (use-package company-restclient
      :ensure t
      :config
      (add-to-list 'company-backends 'company-restclient)))
  :bind ("<f5>" . alkaline/restclient))

(use-package markdown-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

;; (use-package dired-details
;;   :ensure t
;;   :init
;;   (dired-details-install))

;;pdf
(setq doc-view-continuous t)

;;;octave
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

(use-package json-mode
  :ensure t)
(use-package json-reformat
  :ensure t)


(use-package tt-mode
  :ensure t)

(use-package git-messenger
  :ensure t
  :bind("C-x v p" . git-messenger:popup-message))

(use-package edit-server
  :ensure t
  :init
  (edit-server-start))

(use-package ess
  :ensure t)

(use-package osx-pseudo-daemon
  :ensure t
  :init
  (osx-pseudo-daemon-mode))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(projectile-project-root-files
   (quote
    ("rebar.config" "project.clj" "SConstruct" "pom.xml" "build.sbt" "build.gradle" "Gemfile" "requirements.txt" "setup.py" "tox.ini" "package.json" "gulpfile.js" "Gruntfile.js" "bower.json" "composer.json" "Cargo.toml" "mix.exs" "zlo.json")))
 '(projectile-tags-command
   "/home/alkaline/bin/ctags --exclude=desktop.bundles/direct -Re -f \"%s\" %s"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
