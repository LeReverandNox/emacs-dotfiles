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

;; Load evil-mode
(use-package evil
  :ensure t
  :config
  (evil-mode t))
;; Load powerline
(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))
;; Load helm
(use-package helm
  :ensure t
  :bind (
	 ("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-x b" . helm-mini)
	 ("C-x C-b" . helm-mini))
  :config
  (helm-mode t))
(use-package helm-descbinds
  :ensure t
  :bind ("C-h b" . helm-descbinds))

;; Load evil-mc
(use-package evil-mc
  :ensure t
  :bind (
	 ("C->" . evil-mc-make-and-goto-next-match)
	 ("C-<" . evil-mc-make-and-goto-prev-match)
	 ("C-g" . evil-mc-undo-all-cursors))
  :config
  (global-evil-mc-mode 1))

;; Load Magit
(use-package magit
  :ensure t
  :bind (
	 ("C-x g" . magit-status))
  :config
  (use-package evil-magit))

(use-package git-gutter-fringe
  :ensure t
  :config
  (global-git-gutter-mode 1))
