;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface customizations

;; Turn off start message
(setq inhibit-startup-message t)

;; Turn off menu bar
(menu-bar-mode -1)

;; Turn off tool bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Turn off scroll bar
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Turn on linum-mode for all buffers
(global-linum-mode t)

;; Turn off cursor blinking and bell
(blink-cursor-mode 0)
(setq ring-bell-function 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Adjust font size and line spacing
(set-face-attribute 'default nil :height 100)
(setq-default line-spacing 0.2)

;; Load color theme (Must be intalled first).
;; Can be added to the list of packages to auto install in "../init.el"
(load-theme 'atom-one-dark t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Setup how Emacs interacts with OS.
(setq
 ;; Commands C-w and C-y to use the clipboard selection.
 x-select-enable-clipboard t
 ;; Commands C-w and C-y to use the primary selection.
 x-select-enable-primary t
 ;; Save clipboard strings into kill ring before replacing them.
 save-interprogram-paste-before-kill t
 ;; Apropos commands behave as if they had been given a prefix argument.
 apropos-do-all t
 ;; Yank at point regardless of click position.
 mouse-yank-at-point t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
