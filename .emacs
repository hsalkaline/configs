;; melpa packages
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t))

(let ((default-directory "~/.emacs.d/github/"))
    (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/alkaline/"))
;;install missing packages
(autoload 'alkaline/install-missing-packages "packages" "missing packages installer" t)

;;colors
(load-theme 'tangotango t)

;; lang/coding
(set-language-environment 'Russian)
(prefer-coding-system 'utf-8)

;; general
(defalias 'yes-or-no-p 'y-or-n-p)
(setq dired-use-ls-dired nil)

;; no menu
(menu-bar-mode -1)
(tool-bar-mode -1)

; line numbers
(setq linum-format "%s ")
(global-linum-mode t)
;; (column-number-mode t)

; auto-save
(setq auto-save-visited-file-name t)

;restore session
(desktop-save-mode t)

; kill without confirm
(defun alkaline/kill-this-buffer-volatile ()
  (interactive)
  (set-buffer-modified-p nil)
  (kill-this-buffer))
(global-set-key (kbd "C-x C-k") 'alkaline/kill-this-buffer-volatile)
(global-set-key (kbd "C-x k") 'alkaline/kill-this-buffer-volatile)

(defun alkaline/cleanup-buffer-safe ()
    (interactive)
    (untabify (point-min) (point-max))
    (delete-trailing-whitespace)
    (set-buffer-file-coding-system 'utf-8))

;; trail whitespace and convert tabs to spaces on save
(add-hook 'before-save-hook 'alkaline/cleanup-buffer-safe)

;; eshell
(global-set-key (kbd "<f8>") 'eshell)

; commenting \M-;
(defun comment-eclipse ()
  (interactive)
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (when (or (not transient-mark-mode) (region-active-p))
      (setq start (save-excursion
                    (goto-char (region-beginning))
                    (beginning-of-line)
                    (point))
            end (save-excursion
                  (goto-char (region-end))
                  (end-of-line)
                  (point))))
    (comment-or-uncomment-region start end)))
(global-set-key "\M-;" 'comment-eclipse)

;backups in temp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

; replace in region
(delete-selection-mode t)

; smart-beginning-of-line
(defun alkaline/smart-line-beginning ()
    "Move point to the beginning of text on the current line; if that is already
the current position of point, then move it to the beginning of the line."
    (interactive)
    (let ((pt (point)))
      (beginning-of-line-text)
      (when (eq pt (point))
        (beginning-of-line))))
(global-set-key "\C-a" 'alkaline/smart-line-beginning)

;; key-chord
(key-chord-mode t)
(global-set-key (kbd "M-k") 'key-chord-mode)

; evil-mode
(require 'evil)
;; (key-chord-define-global "dd" 'evil-delete-whole-line)
(global-set-key "\C-d" 'evil-delete-whole-line)
(key-chord-define-global "yy" 'evil-yank-line)
(global-set-key "\C-v" 'set-mark-command)
(global-set-key (kbd "C-S-v") 'set-mark-command)

(defun alkaline/vi-open-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun alkaline/vi-open-line-below ()
  "Insert a newline below the current line and put point at beginning."
  (interactive)
  (unless (eolp)
    (end-of-line))
    (newline-and-indent))

(global-set-key "\C-l" 'evil-forward-char)
(global-set-key "\C-h" 'evil-backward-char)
(global-set-key "\C-k" 'evil-previous-line)
(global-set-key "\C-j" 'evil-next-line)

(global-set-key (kbd "C-o") 'alkaline/vi-open-line-below)
(global-set-key (kbd "C-S-o") 'alkaline/vi-open-line-above)

;;jumping
(require 'cl)
(load "better-jump.el")
(setq bjump-picker-single-letter-list "asdfghjklqwertyuiopzxcvbnmASDFGHJKLQWERTYUIOPZXCVBNM")
(defun alkaline/visual-select-to-char ()
  (interactive)
  (call-interactively 'set-mark-command)
  (call-interactively 'bjump-char-jump)
  (forward-char))
(global-set-key "\C-f" 'alkaline/visual-select-to-char)

;;selecting
(global-set-key "\C-q" 'er/expand-region)
(global-set-key "\M-q" 'mc/mark-all-dwim)
(global-set-key "\M-e" 'mc/mark-next-like-this)
(global-set-key "\M-d" 'mc/edit-lines)

;; popwin
(require 'popwin)
(popwin-mode 1)

;; smartparens
(require 'smartparens-config)
(smartparens-global-mode t)

;; autocomplete
(add-hook 'after-init-hook 'global-company-mode)
;; (require 'auto-complete-config)
;; (ac-config-default)
;; (setq ac-dwim nil)

;; undo-tree \C-x u
(global-undo-tree-mode)
(setq undo-tree-history-directory-alist `((".*" . "~/.emacs.d/undo")))
(setq undo-tree-auto-save-history t)

;;previous-buffer
(defun alkaline/switch-to-previous-buffer ()
    (interactive)
      (switch-to-buffer (other-buffer (current-buffer) 1)))

(global-set-key (kbd "<backtab>") 'alkaline/switch-to-previous-buffer)

;; org-mode
;; (global-set-key "\C-cl" 'org-store-link)
;; (global-set-key "\C-ca" 'org-agenda)
;; (global-set-key "\C-cb" 'org-iswitchb)

;; js-mode, coffee-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(require 'tern)
(define-key tern-mode-keymap (kbd "\C-c\C-c") nil)
(define-key tern-mode-keymap (kbd "\C-c\C-d") nil)

(add-hook 'js2-mode-hook #'(lambda () (tern-mode t)))
(require 'company)
(add-to-list 'company-backends 'company-tern)

(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(add-hook 'js2-mode-hook
          #'(lambda ()
              (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
              (define-key js2-mode-map "@" 'js-doc-insert-tag)))

;; ido
(ido-mode t)

;; \C-c\C-w copy regexp
;; \C-c\C-q quit
;; normal escaping in re-builder
(require 're-builder)
(setq reb-re-syntax 'string)

;; recently opened files \C-x\C-r
(require 'recentf)
(recentf-mode t)
(global-set-key "\C-x\C-r" 'helm-recentf)

;; helm \M-x
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-a") 'helm-buffers-list)
(global-set-key (kbd "\C-x\C-b") 'helm-buffers-list)
(global-set-key (kbd "\C-xb") 'helm-mini)
(setq helm-locate-fuzzy-match t
      helm-M-x-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; ace
(global-ace-isearch-mode t)
(setq ace-isearch-input-length 2)
(setq ace-isearch-submode 'ace-jump-char-mode)

;; projectile \C-c p p
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(global-set-key (kbd "\C-p") 'helm-projectile)
(global-set-key (kbd "\C-cf") 'helm-projectile-ag)
(projectile-global-mode t)
(setq projectile-switch-project-action 'helm-projectile)

;; magit
(global-set-key (kbd "\C-c\C-m") 'magit-status)

;; wgrep
(require 'wgrep)
(require 'wgrep-ag)
(autoload 'wgrep-ag-setup "wgrep-ag")
(add-hook 'ag-mode-hook 'wgrep-ag-setup)
(setq wgrep-auto-save-buffer t)
(global-set-key "\C-cf" 'ag-project)
(define-key dired-mode-map (kbd "C-c C-p") 'wdired-change-to-wdired-mode)

(put 'narrow-to-region 'disabled nil)

(global-set-key (kbd "<f5>") 'project-explorer-open)

;;tramp
(setq tramp-default-method "ssh")

;;flyspell
(require 'flyspell)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; insert-file-name
(defun alkaline/insert-file-name (filename &optional args)
  "Insert name of file FILENAME into buffer after point.

  Prefixed with \\[universal-argument], expand the file name to
  its fully canocalized path.  See `expand-file-name'.

  Prefixed with \\[negative-argument], use relative path to file
  name from current directory, `default-directory'.  See
  `file-relative-name'.

  The default with no prefix is to insert the file name exactly as
  it appears in the minibuffer prompt."
  (interactive `(,(ido-read-file-name "File Name: ")
                 ,current-prefix-arg))
  (cond ((eq '- args)
         (insert (expand-file-name filename)))
        ((not (null args))
         (insert filename))
        (t
         (insert (file-relative-name filename)))))

(global-set-key "\C-c\C-i" 'alkaline/insert-file-name)

;; screen
(when (not 'display-graphic-p)
  (defun terminal-init-screen ()
    "Terminal initialization function for screen-256color."
    (load "term/xterm")
    (xterm-register-default-colors)
    (tty-set-up-initial-frame-faces))
  (terminal-init-screen))

;;smooth scroll
(require 'smooth-scrolling)

;;powerline
;; (require 'powerline)
(powerline-default-theme)

;;rainbow delimeters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;ffap
(global-set-key "\C-x\C-f" 'find-file-at-point)
