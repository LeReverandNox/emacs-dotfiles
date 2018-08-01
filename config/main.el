;; Put backup files neatly away
(let ((backup-dir (concat user-emacs-directory "tmp/backups"))
      (auto-saves-dir (concat user-emacs-directory "tmp/auto-saves")))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
    auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
    auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
    tramp-backup-directory-alist `((".*" . ,backup-dir))
    tramp-auto-save-directory auto-saves-dir))

(setq backup-by-copying t    ; Don't delink hardlinks
      delete-old-versions t  ; Clean up the backups
      version-control t      ; Use version numbers on backups,
      kept-new-versions 5    ; keep some new versions
      kept-old-versions 2)   ; and some old ones, too

;; Set encoding to UTF-8
(set-language-environment "UTF-8")

;; Set default method for TRAMP
(setq tramp-default-method "ssh")

;; Show trailing whitespaces
(setq-default show-trailing-whitespace t)

;; Set a cutom undo directory, and enable persistent undo
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/tmp/undo")))
(setq undo-tree-auto-save-history t)

;; Enable some sensible-defaults (by @hrs) settings
(sensible-defaults/shorten-yes-or-no)
(sensible-defaults/show-matching-parens)
(sensible-defaults/delete-trailing-whitespace)
(sensible-defaults/make-scripts-executable)
(sensible-defaults/overwrite-selected-text)
(sensible-defaults/ensure-that-files-end-with-newline)
(sensible-defaults/confirm-closing-emacs)
(sensible-defaults/quiet-startup)
(sensible-defaults/always-highlight-code)
(sensible-defaults/refresh-buffers-when-files-change)
(sensible-defaults/bind-commenting-and-uncommenting)

;; Stole some @hrs cool functions
(defun hrs/kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") 'hrs/kill-current-buffer)

;; Always indent with spaces
(setq-default indent-tabs-mode nil)

(defun hrs/generate-scratch-buffer ()
  "Create and switch to a temporary scratch buffer with a random
     name."
  (interactive)
  (switch-to-buffer (make-temp-name "scratch-")))
