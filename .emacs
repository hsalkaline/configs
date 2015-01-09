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

(defun kill-this-buffer-volatile ()
  "Kill current buffer, even if it has been modified."
  (interactive)
  (set-buffer-modified-p nil)
  (kill-this-buffer))
(global-set-key (kbd "C-x k") 'kill-this-buffer-volatile)

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

(delete-selection-mode t)

;; expand-region
(global-set-key (kbd "\C-c=") 'er/expand-region)

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

;; projectile \C-c p p
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(global-set-key (kbd "\C-c\C-c") 'helm-projectile)
(global-set-key (kbd "\C-cf") 'helm-projectile-ag)
(projectile-global-mode t)
(setq projectile-switch-project-action 'helm-projectile)

;; magit
(global-set-key (kbd "\C-x\C-g") 'magit-status)
