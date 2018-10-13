;; Load sensible-defaults
(load "~/.emacs.d/config/sensible-defaults/sensible-defaults.el")

;; Load all the packages
(load "~/.emacs.d/config/packages.el")

;; Load the main config file
(load "~/.emacs.d/config/main.el")

;; Load style relative settings
(load "~/.emacs.d/config/style.el")

;; Load the 42 header config
(load "~/.emacs.d/config/42/list.el")
(load "~/.emacs.d/config/42/string.el")
(load "~/.emacs.d/config/42/comments.el")
(load "~/.emacs.d/config/42/header.el")

;; Specify a file for customize to write into...
(setq custom-file "~/.emacs.d/config/custom.el")
;; ...and load it at startup
(load custom-file)
