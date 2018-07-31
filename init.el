;; Load all the packages
(load "~/.emacs.d/packages.el")

;; Set encoding to UTF-8
(set-language-environment "UTF-8")

;; Specify a file for customize to write into...
(setq custom-file "~/.emacs.d/custom.el")
;;; ...and load it at startup
(load custom-file)

;; Hide menu bar
(menu-bar-mode -1)
;; Hide the toolbar
(tool-bar-mode -1)

;; Display line numbers
(setq linum-format "%d ")
(global-linum-mode 1)

;; Set custom directory for themes
(setq custom-theme-directory (concat user-emacs-directory "themes"))

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

;; Use a visual warning instead of the bell
 (setq visible-bell 1)

;; Background transparency
;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;;(set-frame-parameter (selected-frame) 'alpha <both>)
(set-frame-parameter (selected-frame) 'alpha '(90 . 80))
(add-to-list 'default-frame-alist '(alpha . (90 . 80)))

;; Load a theme
(load-theme 'earthsong t)

;; Disable the welcome screen
(setq inhibit-startup-screen t)

;; Set default method for TRAMP
(setq tramp-default-method "ssh")
