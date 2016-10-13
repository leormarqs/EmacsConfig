;;; editing.el --- Emacs Editing Customizations

;; Filename: editing.el
;; Description: Emacs Editings setup
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

;; Highlights matching parenthesis.
(show-paren-mode 1)

;; Highlight current line.
(global-hl-line-mode 1)

;; Interactive search key bindings.
(global-set-key (kbd "C-s") 'isearch-forward)
(global-set-key (kbd "C-r") 'isearch-backward)
(global-set-key (kbd "C-M-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-M-r") 'isearch-backward-regexp)

;; Automatic indent key bind.
(define-key global-map (kbd "RET") 'newline-and-indent)

;;Align Region
(global-set-key (kbd "C-x a r") 'align-regexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck provides on-the-fly syntax checking for buffers.
;; It runs various linting tools and services to automatically check the
;; contents of buffers while you are typing, and reports warnings and errors
;; directly in the buffer, in the mode line and in an optional error list. Needs
;; additionally external syntax checking programs for the languages you use.
;; http://www.flycheck.org/

;; Enable flycheck globally
(add-hook 'after-init-hook 'global-flycheck-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'apropos)

;; Setup how Emacs interacts with OS.
(setq
 ;; Commands C-w and C-y to use the clipboard selection.
 x-select-enable-clipboard t
 ;; Commands C-w and C-y to use the primary selection.
 x-select-enable-primary t
 ;; Apropos commands behave as if they had been given a prefix argument.
 apropos-do-all t
 ;; Yank at point regardless of click position.
 mouse-yank-at-point t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Better commands for cutting
(defun kill-line-or-region (&optional prefix)
  "Behave as `kill-region PREFIX' if `mark-active' is true, otherwise as `kill-line PREFIX'."
  (interactive "*P")
  (if mark-active
      (kill-region (region-beginning) (region-end) prefix)
    (kill-line prefix)))

(global-set-key (kbd "C-k") 'kill-line-or-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'editing)
;;; editing.el ends here
