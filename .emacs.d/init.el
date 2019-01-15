(require 'package)
(package-initialize)
(setq package-enable-at-startup nil)

(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
			 ("org" . "http://orgmode.org/elpa/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa-stable" . "http://stable.melpa.org/packages/")))

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(magit . "melpa-stable") t)

(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)

(use-package evil-leader
      :commands (evil-leader-mode)
      :ensure t
      :demand evil-leader
      :init
      (global-evil-leader-mode)
      :config
      (progn
	(evil-leader/set-leader "SPC")
	;; bindings from earlier
	(evil-leader/set-key
	  "," 'other-window
	  "a" 'helm-projectile-ag
	  "b" 'helm-buffers-list
	  "d" 'ranger
	  "e" 'pp-eval-last-sexp
	  "f" 'helm-projectile-find-file
	  "h" 'split-window-below
	  "o" 'delete-other-windows
	  "p" 'projectile-switch-project
	  "q" 'delete-window
	  "v" 'split-window-right
	  "s" 'save-buffer
	  "SPC" 'helm-M-x
	  )))

(setq gc-cons-threshold 100000000)

(keyboard-translate ?\C-x ?\C-a)
(keyboard-translate ?\C-a ?\C-x)

;; need to require Evil-Leader and run (global-evil-leader-mode) before Evil is loaded,
;; or otherwise it won’t work in buffers like *scratch* and *messages*.
(global-evil-leader-mode)

;; evil sutffs
(use-package evil
  :ensure t
  :config
  (evil-mode 1))

(add-to-list 'load-path (concat user-emacs-directory "config"))
(require 'my-ibuffer)

;; dired
(eval-after-load 'dired
  '(progn
     (evil-set-initial-state 'dired-mode 'normal)
     (defun my-dired-up-directory ()
       "Take dired up one directory, but behave like dired-find-alternate-file"
       (interactive)
       (let ((old (current-buffer)))
	 (dired-up-directory)
	 (kill-buffer old)))
     (evil-define-key 'normal dired-mode-map
       "h" 'my-dired-up-directory
       "l" 'dired-find-alternate-file
       "v" 'dired-toggle-marks
       "m" 'dired-mark
       "u" 'dired-unmark
       "U" 'dired-unmark-all-marks
       "c" 'dired-create-directory
       "n" 'evil-search-next
       "N" 'evil-search-previous
       "q" 'kill-this-buffer)))

(require 'dired-x)

(use-package projectile
  :ensure t
  :config (progn
            (projectile-mode +1)))
(use-package helm-projectile
  :ensure t
  :config (progn
            (helm-projectile-on)))

;; magit
(use-package magit
  :ensure t
  :config
  (progn
    (evil-set-initial-state 'magit-mode 'normal)
    (evil-set-initial-state 'magit-status-mode 'normal)
    (evil-set-initial-state 'magit-diff-mode 'normal)
    (evil-set-initial-state 'magit-log-mode 'normal)
    (evil-define-key 'normal magit-mode-map
      "j" 'magit-goto-next-section
      "k" 'magit-goto-previous-section)
    (evil-define-key 'normal magit-log-mode-map
      "j" 'magit-goto-next-section
      "k" 'magit-goto-previous-section)
    (evil-define-key 'normal magit-diff-mode-map
      "j" 'magit-goto-next-section
      "k" 'magit-goto-previous-section)))

(if (eq system-type 'darwin)
    (use-package exec-path-from-shell
      :ensure t))

(use-package helm
  :ensure t)
(require 'helm-config)

(use-package ag
  :ensure t)
(use-package helm-ag
  :ensure t)
(use-package paredit
  :ensure t)
(use-package clojure-mode
  :ensure t)
(use-package clojure-mode-extra-font-locking
  :ensure t)
(use-package cider
  :ensure t)
(use-package yaml-mode
  :ensure t)
(use-package go-mode
  :ensure t)
(use-package go-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)
(use-package auto-complete ;;autocompletion on tab
  :ensure t
  :diminish auto-complete-mode
  :init
  (progn
    (global-auto-complete-mode t))
  :bind (:map ac-completing-map
              ("C-j" . ac-next)
              ("C-k" . ac-previous))
  :config
  (progn
    (use-package auto-complete-config)
    (ac-config-default)
    (setq ac-delay 0)
    (setq ac-use-menu-map t)
    (setq ac-dwim t)
    (setq ac-use-fuzzy t)))
(use-package flycheck ;;error checking
  :ensure t
  :diminish flycheck-mode
  :init
  (global-flycheck-mode))

(use-package ido-completing-read+
  :ensure t)
(use-package flx
  :ensure t)
(use-package flx-ido
  :ensure t)
(use-package rainbow-delimiters
  :ensure t)
(use-package spacemacs-theme
  :defer t
  :init (load-theme 'spacemacs-dark t))
(use-package tagedit
  :ensure t)
(use-package ranger
  :ensure t)
(use-package powerline
  :ensure t)
(use-package airline-themes
  :ensure t
  :config (progn
            (require 'airline-themes)
            (load-theme 'airline-badwolf)))
(use-package all-the-icons ;;adding nice icon support to modeline
  :ensure t
  :config
  (setq inhibit-compacting-font-caches t) ;;make all-the-icons load faster
  )
(use-package all-the-icons-dired
  :ensure t
  :diminish all-the-icons-dired-mode
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(ranger-override-dired-mode t)

(add-to-list 'load-path (concat user-emacs-directory "vendor"))

(load "shell-integration.el")
(load "navigation.el")
(load "ui.el")

(load "editing.el")

; Langauage-specific
(load "setup-clojure.el")
(load "elisp-editing.el")
(load "setup-js.el")

(load "misc.el")

; (add-to-list 'load-path "~/.emacs.d/emacs-surround")
; (require 'emacs-surround)
; (global-set-key (kbd "C-g") 'emacs-surround)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("946e871c780b159c4bb9f580537e5d2f7dba1411143194447604ecbaf01bd90c" "8d5f22f7dfd3b2e4fc2f2da46ee71065a9474d0ac726b98f647bc3c7e39f2819" "73a13a70fd111a6cd47f3d4be2260b1e4b717dbf635a9caee6442c949fad41cd" "721bb3cb432bb6be7c58be27d583814e9c56806c06b4077797074b009f322509" "b59d7adea7873d58160d368d42828e7ac670340f11f36f67fa8071dbf957236a" default)))
 '(package-selected-packages
   (quote
    (auto-complete helm-ag ag paredit projectile evil-leader ## evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
