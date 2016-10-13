;;; navigation.el --- Emacs Navigation Customizations

;; Filename: navigation.el
;; Description: Emacs Navigations setup
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
;; Recentf is a minor mode that builds a list of recently opened files.
;; This list is is automatically saved across sessions on exiting Emacs -
;; you can then access this list through a command or the menu.

;; Turn on recent files mode
(require 'recentf)
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(recentf-mode 1)
(setq recentf-max-menu-items 40)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs has excellent interactively do thing mode (ido-mode). It greatly
;; improves navigation experience. For example, to switch between buffers,
;; press C-b x, then type some characters from a buffer name to quickly
;; filter buffers.
;; http://www.emacswiki.org/emacs/InteractivelyDoThings

;; Turn on interactively do thing mode
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length -1)
(setq ido-use-virtual-buffers t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido-ubiquitous mode allows ido usage in as many contexts as possible.

;; Turn on ido-ubiquitous mode (Must be installed first)
;; Can be added to the list of packages to auto install in "../init.el"
(ido-ubiquitous-mode 1)

;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smex is a M-x enhancement for Emacs. Built on top of ido, it provides a
;; convenient interface to your recently and most frequently used commands.
;; And to all the other commands, too.
;; http://www.emacswiki.org/emacs/Smex

;; Turn on smex mode (Must be installed first)
;; Can be added to the list of packages to auto install in "../init.el"
(require 'smex)
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile provides outstanding project management features.
;; http://batsov.com/projectile/

;; Turn on Projectille mode
(projectile-global-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'navigation)
;;; navigation.el ends here
