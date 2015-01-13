;; melpa packages
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
)

;;install missing packages
(load "~/.emacs.d/packages.el")

;; lang/coding
(set-language-environment 'Russian)
(prefer-coding-system 'utf-8)

;; general
(defalias 'yes-or-no-p 'y-or-n-p)

; line numbers
(require 'linum-relative)
(linum-on)
(global-linum-mode t)

; kill without confirm
(defun kill-this-buffer-volatile ()
  "Kill current buffer, even if it has been modified."
  (interactive)
  (set-buffer-modified-p nil)
  (kill-this-buffer))
(global-set-key (kbd "C-x k") 'kill-this-buffer-volatile)

(defun cleanup-buffer-safe ()
    "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
    (interactive)
    (untabify (point-min) (point-max))
    (delete-trailing-whitespace)
    (set-buffer-file-coding-system 'utf-8))

;; trail whitespace and convert tabs to spaces on savekj
(add-hook 'before-save-hook 'cleanup-buffer-safe)

(defun cleanup-buffer ()
    "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
    (interactive)
    (cleanup-buffer-safe)
    (indent-region (point-min) (point-max)))

(global-set-key (kbd "C-c n") 'cleanup-buffer)

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
(evil-mode t)
(key-chord-mode t)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)

;; expand-region
(global-set-key (kbd "\C-c=") 'er/expand-region)

;; smartparens
(require 'smartparens-config)
(smartparens-global-mode t)

;; autocomplete
(require 'auto-complete-config)
(ac-config-default)

;; undo-tree \C-x u
(global-undo-tree-mode)

;; org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
      (global-set-key "\C-cb" 'org-iswitchb)

;; js-mode, coffee-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook
          #'(lambda ()
              (tern-mode t)
              (define-key tern-mode-keymap (kbd "\C-c\C-c") nil)
              (define-key tern-mode-keymap (kbd "\C-c\C-d") nil)))
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

;; less-css
(require 'less-css-mode)
(define-key less-css-mode-map (kbd "\C-c\C-c") nil)

;; ido
(require 'uniquify)
(require 'ido)
(ido-mode t)

;; \C-c\C-w copy regexp
;; \C-c\C-q quit
;; normal escaping in re-builder
(require 're-builder)
(setq reb-re-syntax 'string)

;; recently opened files \C-x\C-r
(require 'recentf)
(recentf-mode t)
(global-set-key "\C-x\C-r" 'recentf-open-files)

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

;; projectile \C-c p p
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(global-set-key (kbd "\C-c\C-c") 'helm-projectile)
(global-set-key (kbd "\C-cf") 'helm-projectile-ag)
(projectile-global-mode t)
(setq projectile-switch-project-action 'helm-projectile)

;; magit
(global-set-key (kbd "\C-x\C-g") 'magit-status)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js2-bounce-indent-p nil)
 '(linum-relative-format "%3s ")
 '(undo-tree-auto-save-history t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
