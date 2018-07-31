;; Set custom directory for themes
(setq custom-theme-directory (concat user-emacs-directory "themes"))

;; Load a theme
(load-theme 'earthsong t)

;; Hide menu bar
(menu-bar-mode -1)
;; Hide the toolbar
(tool-bar-mode -1)
;; Hide scroll bar
(toggle-scroll-bar -1)

;; Display line numbers
(global-display-line-numbers-mode t)

;; Use a visual warning instead of the bell
 (setq visible-bell 1)

;; Background transparency
;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;;(set-frame-parameter (selected-frame) 'alpha <both>)
(set-frame-parameter (selected-frame) 'alpha '(90 . 80))
(add-to-list 'default-frame-alist '(alpha . (90 . 80)))

;; Disable the welcome screen
(setq inhibit-startup-screen t)
