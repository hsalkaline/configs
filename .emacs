;; melpa packages
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
)

;;install missing packages
(load "~/.emacs.d/packages.el")

(load "~/.emacs.d/github/better-jump/better-jump.el")

;; lang/coding
(set-language-environment 'Russian)
(prefer-coding-system 'utf-8)

;; general
(defalias 'yes-or-no-p 'y-or-n-p)
(setq dired-use-ls-dired nil)

; line numbers
(setq linum-format "%s ")
(global-linum-mode t)

; auto-save
(setq auto-save-visited-file-name t)

; kill without confirm
(defun alkaline/kill-this-buffer-volatile ()
  "Kill current buffer, even if it has been modified."
  (interactive)
  (set-buffer-modified-p nil)
  (kill-this-buffer))
(global-set-key (kbd "C-x k") 'alkaline/kill-this-buffer-volatile)

(defun alkaline/cleanup-buffer-safe ()
    "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
    (interactive)
    (untabify (point-min) (point-max))
    (delete-trailing-whitespace)
    (set-buffer-file-coding-system 'utf-8))

;; trail whitespace and convert tabs to spaces on save
(add-hook 'before-save-hook 'alkaline/cleanup-buffer-safe)

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

; evil-mode
(require 'evil)
(key-chord-mode t)
(key-chord-define-global "dd" 'evil-delete-whole-line)
(key-chord-define-global "yy" 'evil-yank-line)

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

;; (global-set-key "\C-l" 'evil-forward-char)
;; (global-set-key "\C-h" 'evil-backward-char)
;; (global-set-key "\C-k" 'evil-previous-line)
;; (global-set-key "\C-j" 'evil-next-line)

(global-set-key (kbd "C-o") 'alkaline/vi-open-line-below)
(global-set-key (kbd "C-S-o") 'alkaline/vi-open-line-above)

;;jumping
(require 'better-jump)
(setq bjump-picker-single-letter-list "asdfghjklqwertyuiopzxcvbnmASDFGHJKLQWERTYUIOPZXCVBNM")
(defun alkaline/visual-select-to-char ()
  (interactive)
  (call-interactively 'set-mark-command)
  (call-interactively 'bjump-char-jump)
  (forward-char))

(key-chord-define-global "ff" 'alkaline/visual-select-to-char)

;;selecting
(global-set-key "\C-q" 'er/expand-region)
(global-set-key "\M-q" 'mc/mark-next-like-this)
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
(setq undo-tree-history-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq undo-tree-auto-save-history t)

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

(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(add-hook 'js2-mode-hook
          #'(lambda ()
              (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
              (define-key js2-mode-map "@" 'js-doc-insert-tag)))

;; ido
;; (require 'uniquify)
;; (require 'ido)
;; (ido-mode t)

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
(global-set-key (kbd "\C-x\C-b") 'helm-buffers-list)
(global-set-key (kbd "\C-xb") 'helm-mini)
(setq helm-locate-fuzzy-match t
      helm-M-x-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; ace
(global-ace-isearch-mode t)

;; projectile \C-c p p
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(global-set-key (kbd "\C-p") 'helm-projectile)
(global-set-key (kbd "\C-cf") 'helm-projectile-ag)
(projectile-global-mode t)
(setq projectile-switch-project-action 'helm-projectile)

;; magit
(global-set-key (kbd "\C-c\C-g") 'magit-status)
