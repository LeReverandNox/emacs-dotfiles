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

;; Delete trailing whitespaces on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Add a blank line at the end of a file before save
(setq require-final-newline t)
