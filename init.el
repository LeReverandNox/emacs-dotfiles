;; Load the main config file
(load "~/.emacs.d/config/main.el")

;; Load all the packages
(load "~/.emacs.d/config/packages.el")

;; Load style relative settings
(load "~/.emacs.d/config/style.el")

;; Specify a file for customize to write into...
(setq custom-file "~/.emacs.d/config/custom.el")
;; ...and load it at startup
(load custom-file)
