;;; init.el -- Emacs Initial Setup

;; Filename: init.el
;; Description: Emacs setup
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

;; Refs.:
;; https://www.emacswiki.org/emacs/init-mode.el
;; http://manenko.com/2016/03/01/setup-emacs-for-rust-development.html
;; https://bassam.co/emacs/2015/08/24/rust-with-emacs/

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package Management

;; Define package repositories
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("gnu" . "http://elpa.gnu.org/packages/") t)

;; Load and activate emacs packages
(package-initialize)

;; Download list of packages available from ELPA (Emacs Lisp Package Archive).
;; Learn about it on http://www.emacswiki.org/emacs/ELPA
(when (not package-archive-contents)
  (package-refresh-contents))

;; Automatic download packages
(defvar my-packages
  '(ido-ubiquitous
    smex
    flycheck
    projectile
    company
    ))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
  (add-to-list 'my-packages 'exec-path-from-shell))

;; Iterate over the list of packages and install
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Setup exec-path-from-shell package for Mac
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
    '("PATH")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizations Managemt

(defvar my-customizations-folder
  (concat user-emacs-directory "customizations/"))

(defvar my-customizations
  '("ui"                 ;; User Interface Commands
    "setup-font"         ;; Default font setup
    "navigation"         ;; Navigation Commands
    "editing"            ;; Editing Commands
    "miscellaneous"      ;; Miscellaneous Commands
    "setup-company-mode" ;; Company Mode Commands
    ))

(dolist (c my-customizations)
  (load (concat my-customizations-folder c ".el")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Show error traces to debug
;;(setq debug-on-error t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init)
;;; init.el ends here
