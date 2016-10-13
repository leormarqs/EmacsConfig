;;; setup-haskell.el -- Haskell Mode Setup

;; Filename: setup-haskell.el
;; Description: Haskell Mode Setup
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

;;User must install the Aspell and dictionarys on your OS.
;;User must install stylish-haskell on your OS.

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Dependencies
(require 'haskell-interactive-mode)
(require 'haskell-process)
(require 'company)
(require 'company-ghc)
(require 'w3m)
(require 'w3m-haddock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;General variables setting
(setq haskell-indentation-electric-flag t)
(setq haskell-process-auto-import-loaded-modules t)
(setq haskell-process-suggest-remove-import-lines t)
(setq haskell-process-type 'stack-ghci)
(setq haskell-stylish-on-save t)
(setq haskell-process-log t)
(setq haskell-tags-on-save t)
(setq haskell-compile-cabal-build-command "stack build")
(setq haskell-compile-cabal-build-alt-command "stack clean --full && stack build")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Setting up haskell interactive mode
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Setting up company backends for haskell
(add-hook 'haskell-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 (append '((company-capf company-dabbrev-code))
                         company-backends))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Cua selection mode enabled to rectangle selection
(add-hook 'haskell-mode-hook (cua-selection-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Auto inserting modules templates on new files
(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Spell check for strings and comments
(add-hook 'haskell-mode-hook 'flyspell-prog-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Loading w3m-haddock on haskell mode
(add-hook 'w3m-display-hook 'w3m-haddock-display)

;;Setting up current project haddock directory
(defvar setup-haskell-local-haddock (shell-command-to-string
				     "stack path --local-doc-root | tr -d '\n'"))
(defvar setup-haskell-working-package (shell-command-to-string
                                       "stack query locals | grep ':$'| sed \"s@:@@\" | tr -d '\n'"))
(defvar setup-haskell-working-version (shell-command-to-string
				       "stack query locals | grep 'version:'| sed \"s@ *version: '@@\" | sed \"s@'@@\" | tr -d '\n'"))

(defvar setup-haskell-working-haddock (concat
			(symbol-value 'setup-haskell-local-haddock)
			"/"
			(symbol-value 'setup-haskell-working-package)
			"-"
			(symbol-value 'setup-haskell-working-version)
			"/index.html"))

(defun project-documentation ()
  "Open the current project documentation."
  (interactive)
  (w3m-browse-url (symbol-value 'setup-haskell-working-haddock)))

;;Setting general haddock files path
(setq haskell-w3m-haddock-dirs
  '("$(stack path --local-doc-root | tr -d '\n')"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Setting up Haskell Align Regions REGEXP
(defvar align-rules-list ())

(add-to-list 'align-rules-list
             '(haskell-types
               (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))

(add-to-list 'align-rules-list
             '(haskell-assignment
               (regexp . "\\(\\s-+\\)=\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))

(add-to-list 'align-rules-list
             '(haskell-arrows
               (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))
(add-to-list 'align-rules-list
             '(haskell-left-arrows
               (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Setting up Haskell mode key-bindings
(eval-after-load 'haskell-mode
  '(progn
     (define-key haskell-mode-map (kbd "C-c C-l")       'haskell-process-load-file)
     (define-key haskell-mode-map (kbd "C-c C-c")       'haskell-compile)
     (define-key haskell-mode-map (kbd "C-c C-z")       'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-s")       'haskell-mode-toggle-scc-at-point)
     (define-key haskell-mode-map (kbd "C-c C-.")       'haskell-mode-format-imports)
     (define-key haskell-mode-map (kbd "C-c C-n C-t")   'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-n C-i")   'haskell-process-do-info)
     (define-key haskell-mode-map (kbd "C-c C-n C-c")   'haskell-process-cabal-build)
     (define-key haskell-mode-map (kbd "C-c C-n c")     'haskell-process-cabal)
     (define-key haskell-mode-map (kbd "C-c <f8>")      'haskell-navigate-imports)
     (define-key haskell-mode-map (kbd "C-c C-d")       'haskell-w3m-open-haddock)
     (define-key haskell-mode-map (kbd "C-c d")         'project-documentation)
     (define-key haskell-mode-map (kbd "M-[")           'align)))

(eval-after-load 'haskell-cabal
  '(progn
     (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
     (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
     (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile)
     (define-key haskell-cabal-mode-map (kbd "C-c c")   'haskell-process-cabal)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-haskell)
;;; setup-haskell.el ends here
