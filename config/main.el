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
(sensible-defaults/set-default-line-length-to 80)

;; Stole some @hrs cool functions
(defun hrs/kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") 'kill-buffer-and-window)

;; Always indent with spaces
(setq-default indent-tabs-mode nil)

(defun hrs/generate-scratch-buffer ()
  "Create and switch to a temporary scratch buffer with a random
     name."
  (interactive)
  (switch-to-buffer (make-temp-name "scratch-")))

;; Treat lower/upper CamelCase as separate words
(global-subword-mode 1)

;; Enable lisp specific modes for... lisp modes
(setq lispy-mode-hooks
      '(clojure-mode-hook
        emacs-lisp-mode-hook
        lisp-mode-hook))

(dolist (hook lispy-mode-hooks)
  (add-hook hook (lambda ()
                   (setq show-paren-style 'expression)
                   (paredit-mode)
                   (rainbow-delimiters-mode))))

;; Always switch to the new pane when splitting
(defun hrs/split-window-below-and-switch ()
  "Split the window horizontally, then switch to the new pane."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun hrs/split-window-right-and-switch ()
  "Split the window vertically, then switch to the new pane."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(define-key evil-window-map "v" 'hrs/split-window-right-and-switch)
(define-key evil-window-map "\C-v" 'hrs/split-window-right-and-switch)

(define-key evil-window-map "s" 'hrs/split-window-below-and-switch)
(define-key evil-window-map "S" 'hrs/split-window-below-and-switch)
(define-key evil-window-map "\C-s" 'hrs/split-window-below-and-switch)
(define-key evil-window-map (kbd "C-S-s") 'hrs/split-window-below-and-switch)

;; Allow org-babel to evaluate languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ruby . t)
   (dot . t)
   (shell . t)
   (python . t)
   (gnuplot . t)))

;; Don't ask before evaluating code blocks
(setq org-confirm-babel-evaluate nil)

;; Automatically activate auto-fill for text or org
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; Add a shortcut to disable auto-fil easily
(global-set-key (kbd "C-c q") 'auto-fill-mode)

;; Enable electric-pair-mode to automaticaly write pair of quotes, brackets and stuff
(electric-pair-mode t)

;; Indent by 4 spaces for C files
(setq-default c-basic-offset 4)
(add-hook 'c-mode-hook (lambda ()
                         (setq tab-width 4)
                         (setq evil-shift-width 4)
                         (setq indent-tabs-mode t)))
;; Set C coding to "linux" to doesn't indent braces in C files
(setq-default c-default-style "linux")

;; Enable winner mode to easily go back and forth betweed window configuration
(winner-mode t)

;; Sentence end with a single space
(setq sentence-end-double-space nil)

;; Automatically save/restore Emacs session
(setq desktop-path (list (concat user-emacs-directory "tmp")))
(desktop-save-mode t)

;; Test stuff for C
;; (require 'semantic)
;; (require 'semantic/bovine/gcc)
;; (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
;; (semantic-mode 1)
;; (global-ede-mode t)
;; (ede-enable-generic-projects)
