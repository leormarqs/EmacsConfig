;;; ui.el --- Emacs User Interface Customizations

;; Filename: ui.el
;; Description: Emacs User Interface Setup
;; Author: Leonardo Marques Rodrigues <lmrodrigues@riseup.net>
;; Maintainer: Leonardo Marques Rodrigues <lmrodrigues@riseup.net>
;; Copyright (C) 2016, Leonardo Marques Rodrigues, all rights reserved.
;; Created:
;; Version: 0.1
;; Last-Updated:
;;           By: Leonardo Marques Rodrigues
;; URL: https://github.com/leormarqs/EmacsConfig
;; Keywords:
;; Compatibility: GNU Emacs 24.5.1
;;
;; Features that might be required by this library:
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:


;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; Key Binds to buffer navigation
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Adjust font size and line spacing
(set-face-attribute 'default nil :height 100)
(setq-default line-spacing 0.2)

;; Load color theme (Must be intalled first).
;; Can be added to the list of packages to auto install in "../init.el"
(load-theme 'misterioso t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'apropos)

;; Setup how Emacs interacts with OS.
(setq
 ;; Commands C-w and C-y to use the clipboard selection.
 x-select-enable-clipboard t
 ;; Commands C-w and C-y to use the primary selection.
 x-select-enable-primary t
 ;; Save clipboard strings into kill ring before replacing them.
 save-interprogram-paste-before-kill nil
 ;; Apropos commands behave as if they had been given a prefix argument.
 apropos-do-all t
 ;; Yank at point regardless of click position.
 mouse-yank-at-point t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'ui)
;;; ui.el ends here
