;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs init file
;; Require Emacs > 24.5.1

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
  '(atom-one-dark-theme
    ido-ubiquitous
    smex))

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
;; Customizations Management

(defvar my-customizations-folder
  (concat user-emacs-directory "customizations/"))

(defvar my-customizations
  '("ui" ;; User interface
    "navigation" ;; Navigation commands
    ))

(dolist (c my-customizations)
  (load (concat my-customizations-folder c ".el")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(ansi-color-faces-vector
;;    [default default default italic underline success warning error])
;;  '(column-number-mode t)
;;  '(cua-mode t nil (cua-base))
;;  '(custom-enabled-themes (quote (misterioso)))
;;  '(haskell-process-auto-import-loaded-modules t)
;;  '(haskell-process-log t)
;;  '(haskell-process-suggest-remove-import-lines t)
;;  '(haskell-process-type (quote stack-ghci))
;;  '(show-paren-mode t)
;;  '(tool-bar-mode nil))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;Configuring company
;; ;; Enable company globally for all mode
;; (global-company-mode)

;; ;; Reduce the time after which the company auto completion popup opens
;; (setq company-idle-delay 0.2)

;; ;; Reduce the number of characters before company kicks in
;; (setq company-minimum-prefix-length 1)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
;; (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
;; (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;Configuring Haskell-Mode
;; (eval-after-load 'haskell-mode '(progn
;;   (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
;;   (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
;;   (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
;;   (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
;;   (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
;;   (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
;;   (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)));
;; (eval-after-load 'haskell-cabal '(progn
;;   (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
;;   (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
;;   (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
;;   (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

;; ;;Activing auto-complete 
;; ;(require 'auto-complete)
;; ;(global-auto-complete-mode t)
;; ;(eval-after-load 'auto-complete
;; ;  '(add-to-list 'ac-modes 'haskell-interactive-mode))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;Configuring RUST-MODE

;; ;; Set path to racer binary
;; (setq racer-cmd "~/.cargo/bin/racer")

;; ;; Set path to rust src directory
;; (setq racer-rust-src-path "~/.rust/src/")

;; ;; Load rust-mode when you open `.rs` files
;; (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; ;;turn eldoc on
;; (add-hook 'rust-mode-hook 'turn-on-eldoc-mode)

;; ;; Setting up configurations when you load rust-mode
;; (add-hook 'rust-mode-hook

;; 	  '(lambda ()
;; 	     ;; Enable racer
;; 	     (racer-activate)
  
;; 	     ;; Hook in racer with eldoc to provide documentation
;; 	     (racer-turn-on-eldoc)
	     
;; 	     ;; Use flycheck-rust in rust-mode
;; 	     (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
	     
;; 	     ;; Use company-racer in rust mode
;; 	     (set (make-local-variable 'company-backends) '(company-racer))
	     
;; 	     ;; Key binding to jump to method definition
;; 	     (local-set-key (kbd "M-.") #'racer-find-definition)
     
;; 	     ;; Key binding to auto complete and indent
;; 	     (local-set-key (kbd "TAB") #'racer-complete-or-indent)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
