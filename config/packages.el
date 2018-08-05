;; Setup package, so we can install use-package
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

;; Install use-package if not already present
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))


;; Ensure that packages are present
(setq use-package-always-ensure t)

;; Load evil-mode
(use-package evil
  :init
  (setq evil-want-abbrev-expand-on-insert-exit nil)
  :config
  (evil-mode t))

(use-package evil-surround
  :config
  (global-evil-surround-mode t))

;; Load powerline
(use-package powerline
  :config
  (powerline-default-theme))
;; Load helm
(use-package helm
  :bind (
	 ("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-x b" . helm-mini)
	 ("C-x C-b" . helm-mini))
  :config
  (helm-mode t))
(use-package helm-descbinds
  :bind ("C-h b" . helm-descbinds))

;; Load evil-mc
(use-package evil-mc
  :bind (
	 ("C->" . evil-mc-make-and-goto-next-match)
	 ("C-<" . evil-mc-make-and-goto-prev-match)
	 ("C-g" . evil-mc-undo-all-cursors))
  :config
  (global-evil-mc-mode 1))

;; Load Magit
(use-package magit
  :bind (
	 ("C-x g" . magit-status))
  :config
  (add-hook 'with-editor-mode-hook 'evil-insert-state)
  (use-package evil-magit))

(use-package git-gutter-fringe
  :config
  (global-git-gutter-mode 1))

;; Dimish, so I can hide or rename mode in modeline
(defmacro diminish-minor-mode (filename mode &optional abbrev)
  `(eval-after-load (symbol-name ,filename)
     '(diminish ,mode ,abbrev)))

(defmacro diminish-major-mode (mode-hook abbrev)
  `(add-hook ,mode-hook
             (lambda () (setq mode-name ,abbrev))))

(use-package diminish
  :config
  (diminish-major-mode 'emacs-lisp-mode-hook "el")
  (diminish-major-mode 'python-mode-hook "Py")
  (diminish-major-mode 'js-mode-hook "JS")
  (diminish-major-mode 'sh-mode-hook "Sh")
  (diminish-minor-mode 'simple 'auto-fill-function)
  (diminish-minor-mode 'paredit 'paredit-mode)
  (diminish-minor-mode 'subword 'subword-mode)
  (diminish-minor-mode 'abbrev 'abbrev-mode)
  (diminish-minor-mode 'undo-tree 'undo-tree-mode)
  (diminish-minor-mode 'git-gutter 'git-gutter-mode)
  (diminish-minor-mode 'evil-mc 'evil-mc-mode)
  (diminish-minor-mode 'eldoc 'eldoc-mode)
  (diminish-minor-mode 'helm-mode 'helm-mode))

;; Use Paredit and rainbow-delimiters to facilitate work with lisp files
(use-package paredit)
(use-package rainbow-delimiters)

;; Use AG, The_Silver_Searcher binding for Emacs. Needs the_silver_searcher
;; in order to work
(use-package ag)

;; Use Projectile to handle project easily
(defun hrs/search-project-for-symbol-at-point ()
  "Use `projectile-ag' to search the current project for `symbol-at-point'."
  (interactive)
  (projectile-ag (projectile-symbol-at-point)))

(use-package projectile
  :init
  (setq projectile-require-project-root nil)
  :config
  (global-set-key (kbd "C-c v") 'projectile-ag)
  (global-set-key (kbd "C-c C-v") 'hrs/search-project-for-symbol-at-point))

;; Load org and configure LaTeX export
(use-package org
  :config
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-listings 'minted)
  (setq org-latex-pdf-process
      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  ;; Syntax highlight src blocks in org-mode
  (setq org-src-fontify-natively t)
  ;; Make tab work like in a code buffer for src blocks
  (setq org-src-tab-acts-natively t)
  ;; Use current window when editing a code snippet
  (setq org-src-window-setup 'current-window)
  ;; Log the date when a TODO is DONE
  (setq org-log-done 'time)
  ;; Remove footer when exporting in HTML mode
  (setq org-html-postamble nil))

;; Require ox-md and ox-beamer to export as md or beamer !
(require 'ox-md)
(require 'ox-beamer)
;; Use ox-twbs to export as Boostrap
(use-package ox-twbs)

;; Use org-bullets to have pretty bullets
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook #'org-bullets-mode))

;; Use htmlize to export nicely text decorations
(use-package htmlize)
