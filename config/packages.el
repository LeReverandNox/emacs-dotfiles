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
  (diminish-minor-mode 'projectile 'projectile-mode)
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
  (global-set-key (kbd "C-c C-v") 'hrs/search-project-for-symbol-at-point)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (projectile-mode t))

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

;; Use company-mode to have completions !
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;; Use flycheck to have syntax checking
(use-package flycheck)

;; Python packages
(use-package py-autopep8)
(use-package elpy
  :config
  (elpy-enable)
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (require 'py-autopep8)
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))
(use-package company-jedi
  :config
  (add-to-list 'company-backends 'company-jedi)
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t))

;; Use all-the-icons to have pretty icons !
(use-package all-the-icons)

;; Use neotree to have a tree... NerdTree like
(use-package neotree
  :config
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-change-root)
  (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
  (evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
  (evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
  (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "s") 'neotree-enter-horizontal-split)
  (evil-define-key 'normal neotree-mode-map (kbd "v") 'neotree-enter-vertical-split)
  (evil-define-key 'normal neotree-mode-map (kbd "O") 'neotree-open-file-in-system-application)
  (evil-define-key 'normal neotree-mode-map (kbd "y") 'neotree-copy-filepath-to-yank-ring)
  (evil-define-key 'normal neotree-mode-map (kbd "r") 'neotree-refresh)
  (evil-define-key 'normal neotree-mode-map (kbd "C-b") 'neotree-hide)
  (define-key evil-motion-state-map (kbd "C-b") 'neotree-show)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-window-fixed-size nil))
