;; melpa packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
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

;;ls --dired
(setq dired-use-ls-dired nil)

; line numbers
(setq linum-format "%s ")
(global-linum-mode t)
;; (column-number-mode t)

; auto-save
(setq auto-save-visited-file-name t)

;parens
(show-paren-mode t)

;recentf-mode
(recentf-mode t)

;restore session
(desktop-save-mode t)

;; trail whitespace and convert tabs to spaces on save
(add-hook 'before-save-hook (lambda() (unless (string= (buffer-local-value 'major-mode (current-buffer)) "makefile-bsdmake-mode")
                                        (alkaline/cleanup-buffer-safe))))

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
(ido-mode t)

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
(use-package tangotango-theme
  :ensure t
  :config
  (load-theme 'tangotango t))

;; ;; key-chord
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
  :bind(("C-d" . evil-delete-whole-line)
        ("C-v" . set-mark-command)
        ("C-l" . evil-forward-char)
        ("C-h" . evil-backward-char)
        ("C-k" . evil-previous-line)
        ("C-j" . evil-next-line)
        ("M-j" . evil-join)))

(use-package dot-mode
  :ensure t
  :init
  (progn
    (add-hook 'find-file-hooks 'dot-mode-on)
    (define-key dot-mode-map (kbd "C-'") 'dot-mode-execute)))

;;jumping
(require 'cl)
(use-package ov
  :ensure t)
(use-package dash
  :ensure t)
(use-package better-jump
  :load-path "github/better-jump"
  :config
  (setq bjump-picker-single-letter-list "asdfghjklqwertyuiopzxcvbnmASDFGHJKLQWERTYUIOPZXCVBNM"))

(defun alkaline/visual-select-to-char ()
  (interactive)
  (call-interactively 'set-mark-command)
  (call-interactively 'bjump-char-jump)
  (forward-char))
(global-set-key "\C-f" 'alkaline/visual-select-to-char)

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
  :init (popwin-mode t))

(electric-pair-mode t)

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
    (setq undo-tree-auto-save-history t)))

;; org-mode
;; (global-set-key "\C-cl" 'org-store-link)
;; (global-set-key "\C-ca" 'org-agenda)
;; (global-set-key "\C-cb" 'org-iswitchb)

(use-package js2-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  :config
  (progn
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
                    (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc))))
    (use-package js2-refactor
      :ensure t
      :init
      (require 'js2-refactor)
      :config
      (js2r-add-keybindings-with-prefix "C-c C-m"))))

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
    (use-package helm-ag
      :ensure t)
    (use-package helm-css-scss
      :ensure t))
  :bind (("M-x" . helm-M-x)
         ("M-a" . helm-buffers-list)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         ("M-y" . helm-show-kill-ring)
         ("C-x C-r" . helm-recentf)
         ("M-m" . helm-mark-ring)))

(use-package magit
  :ensure t
  :bind ("C-c m" . magit-status))
  ;; :config
  ;; (use-package magit-filenotify
  ;;   :ensure t
  ;;   :init
  ;;   (add-hook 'magit-status-mode-hook 'magit-filenotify-mode)))

(use-package flyspell
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(use-package smooth-scrolling
  :ensure t)

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(use-package ace-isearch
  :ensure t
  :config
  (progn
    (global-ace-isearch-mode t)
    (setq ace-isearch-input-length 2)
    (setq ace-isearch-submode 'ace-jump-char-mode)))

(use-package alkaline
  :load-path "alkaline")

(use-package floobits)
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

(use-package smooth-scrolling
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
  (add-hook 'prog-mode-hook 'projectile-mode)
  :config
  (progn
  (setq projectile-completion-system 'helm)
  (setq projectile-switch-project-action 'helm-projectile)
  (add-to-list 'projectile-test-files-suffices ".spec" t)
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

;; (use-package smart-forward
;;   :ensure t
;;   :bind(("ESC <up>" . smart-up)
;;         ("ESC <down>" . smart-down)
;;         ("ESC <left>" . smart-backward)
;;         ("ESC <right>" . smart-forward)))
