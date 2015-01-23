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

; commenting \M-;
(defun alkaline/comment-eclipse ()
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
(global-set-key "\M-;" 'alkaline/comment-eclipse)

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

(global-set-key (kbd "C-o") 'alkaline/vi-open-line-below)
(global-set-key (kbd "C-S-o") 'alkaline/vi-open-line-above)

;;previous-buffer
(defun alkaline/switch-to-previous-buffer ()
    (interactive)
      (switch-to-buffer (other-buffer (current-buffer) 1)))

(global-set-key (kbd "<backtab>") 'alkaline/switch-to-previous-buffer)

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

(provide 'alkaline)
