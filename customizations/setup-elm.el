;;; setup-haskell.el -- Haskell Mode Setup

;; Filename: setup-elm.el
;; Description: Elm Mode Setup
;; Author: Leonardo Marques Rodrigues <lmrodrigues@riseup.net>
;; Maintainer: Leonardo Marques Rodrigues <lmrodrigues@riseup.net>
;; Copyright (C) 2017, Leonardo Marques Rodrigues, all rights reserved.
;; Created:
;; Version: 0.1
;; Last-Updated:
;;           By: Leonardo Marques Rodrigues
;; URL: https://github.com/leormarqs/EmacsConfig
;; Keywords:
;; Compatibility: GNU Emacs 24.5.1
;;
;; Features that might be required by this library:
;; UNIX Commands `find` and `egrep` should be on PATH
;; Emacs commands `etags` should be on PATH
;; elm-oracle should be installed
;; elm-format should be installed
;; elm-test should be installed

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
;;Dependencies
(require 'elm-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;General variables setting
(setq elm-tags-on-save t)
(setq elm-tags-exclude-elm-stuff nil)
(setq elm-sort-imports-on-save t)
(setq elm-format-on-save t)

(setq haskell-indentation-electric-flag t)
(setq haskell-process-auto-import-loaded-modules t)
(setq haskell-process-suggest-remove-import-lines t)
(setq haskell-process-type 'stack-ghci)
(setq haskell-stylish-on-save t)
(setq haskell-process-log t)

(setq haskell-compile-cabal-build-command "stack build")
(setq haskell-compile-cabal-build-alt-command "stack clean --full && stack build")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Setting up company backends for haskell
(add-to-list 'company-backends 'company-elm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-elm)
;;; setup-elm.el ends here
