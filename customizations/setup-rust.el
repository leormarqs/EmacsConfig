;;; setup-rust.el -- Rust Mode Setup

;; Filename: setup-rust.el
;; Description: Rust Mode Setup
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
;; Racer allows Emacs to use Racer for Rust code completion and navigation.

;; Configuring Racer and Rust-Src path
(setq racer-cmd "/home/lmrodrigues/.local/bin/racer")

(setq racer-rust-src-path "/home/lmrodrigues/.rust/src/")

(unless (getenv "RUST_SRC_PATH")
  (setenv "RUST_SRC_PATH" "/home/lmrodrigues/.rust/src/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rust-Mode adds Rust syntax highlight, etc.
;; Flycheck-Rust adds Rust syntax checking and linting to Flycheck.

;; Auto Load Rust-Mode
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
;; Auto Load Racer-Mode
;(add-hook 'rust-mode-hook  #'racer-mode)
;; Auto Load ElDoc-Mode
(add-hook 'rust-mode-hook #'eldoc-mode)
;; Auto Load Company-Mode
(add-hook 'rust-mode-hook #'company-mode)
(add-hook 'rust-mode-hook
          '(lambda ()
             (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
	     ;; Key binding to jump to method definition
	     ;; Key binding to auto complete and indent
             (local-set-key (kbd "TAB") #'company-indent-or-complete-common)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-rust)
;;; setup-rust.el ends here
