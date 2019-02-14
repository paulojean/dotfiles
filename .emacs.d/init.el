(require 'package)
(package-initialize)
(setq package-enable-at-startup nil)

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
    (company-mode counsel ivy-posframe flycheck-posframe posframe evil-collection ivy golden-ratio treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil treemacs lua-mode psci feature-mode clomacs evil-magit evil-commentary which-key origami linum-relative nix-mode auto-complete helm-ag ag paredit projectile evil-leader ## evil)))
 '(safe-local-variable-values
   (quote
    ((eval progn
           (require
            (quote projectile))
           (puthash
            (projectile-project-root)
            "cask exec buttercup -L ." projectile-test-cmd-map))))))

(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)

;; evil sutffs
(use-package evil
  :ensure t
  :init (progn
          (setq evil-want-integration t)
          (setq evil-want-keybinding nil)
          )
  :config (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config (evil-collection-init))

(use-package evil-leader
  :commands (evil-leader-mode)
  :ensure t
  :demand evil-leader
  :init
  (global-evil-leader-mode)
  :config
  (progn
    (evil-leader/set-leader "SPC")
    (evil-leader/set-key
      "'" 'linum-mode
      "," 'other-window
      ";" 'linum-relative-toggle
      "SPC" 'helm-M-x
      "TAB" 'switch-to-prev-buffer
      "a" 'helm-projectile-ag
      "b" 'helm-buffers-list
      "d d" 'ranger
      "d o" 'delete-other-windows
      "d p" 'pwd
      "e" 'pp-eval-last-sexp
      "h" 'split-window-below
      "m" 'magit
      "n" 'ibuffer
      "l e" 'flycheck-list-errors
      "o c a" 'origami-close-all-nodes
      "o n" 'origami-toggle-node
      "o o a" 'origami-open-all-nodes
      "p f" 'helm-projectile-find-file-dwim
      "p p" 'helm-projectile-switch-project
      "q" 'delete-window
      "s" 'save-buffer
      "t g" 'golden-ratio-mode
      "v" 'split-window-right
      )))

(setq gc-cons-threshold 100000000)

(keyboard-translate ?\C-x ?\C-a)
(keyboard-translate ?\C-a ?\C-x)

;; need to require Evil-Leader and run (global-evil-leader-mode) before Evil is loaded,
;; or otherwise it won’t work in buffers like *scratch* and *messages*.
(global-evil-leader-mode)

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(eval-after-load 'shell
  (progn
    (evil-define-key 'normal shell-mode-map
      (kbd "C-r") 'shutils-history-helm/show-history)
    (evil-define-key 'insert shell-mode-map
      (kbd "C-r") 'shutils-history-helm/show-history)))

(add-to-list 'load-path (concat user-emacs-directory "config"))
(require 'my-ibuffer)
(use-package ibuffer-projectile
  :ensure t
  :config (progn
            (add-hook 'ibuffer-hook
                      (lambda ()
                        (ibuffer-projectile-set-filter-groups)
                        (unless (eq ibuffer-sorting-mode 'alphabetic)
                          (ibuffer-do-sort-by-alphabetic))))))

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

(use-package magit
  :ensure t)

(use-package evil-magit
  :ensure t
  :after magit
  :init (progn
           (setq evil-magit-want-horizontal-movement t)))

(if (eq system-type 'darwin)
    (use-package exec-path-from-shell
      :ensure t))

(use-package helm
  :ensure t
  :config (require 'helm-config))

(use-package flx
  :ensure t)
(use-package flx-ido
  :ensure t)

(use-package evil-commentary
  :ensure t
  :config (evil-commentary-mode))

(use-package dash
  :ensure t)
(use-package s
  :ensure t)
(use-package ag
  :ensure t)
(use-package helm-ag
  :ensure t)
(use-package helm-projectile
  :ensure t
  :config (progn
            (helm-projectile-on)))

(use-package origami
  :ensure t
  :init (progn
          (require 'origami)
          (global-origami-mode t)))
(use-package paredit
  :ensure t)
(use-package clojure-mode
  :ensure t)
(use-package clojure-mode-extra-font-locking
  :ensure t)
(use-package cider
  :ensure t)
(use-package clomacs
  :ensure t)
(use-package yaml-mode
  :ensure t)
(use-package go-mode
  :ensure t)
(use-package nix-mode
  :ensure t)
(use-package haskell-mode
  :ensure t)
(use-package psc-ide
  :ensure t
  :init (progn
          (add-hook 'purescript-mode-hook
                    (lambda ()
                      (psc-ide-mode)
                      (company-mode)
                      (flycheck-mode)
                      (turn-on-purescript-indentation)))))

(use-package lua-mode
  :ensure t)

(use-package psci
  :ensure t
  :init (progn
          (add-hook 'purescript-mode-hook 'inferior-psci-mode)))

(use-package feature-mode
  :ensure t
  :config (progn
            (setq feature-default-language "en")))
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

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init (progn
          (add-to-list 'display-buffer-alist
                       `(,(rx bos "*Flycheck errors*" eos)
                         (display-buffer-reuse-window
                          display-buffer-in-side-window)
                         (side            . bottom)
                         (reusable-frames . visible)
                         (window-height   . 0.3)))
          (global-flycheck-mode)))
(use-package flycheck-posframe
  :ensure t
  :after flycheck
  :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

(use-package ido-completing-read+
  :ensure t)
(use-package rainbow-delimiters
  :ensure t)
(use-package tagedit
  :ensure t)
(use-package ranger
  :ensure t)

(use-package spaceline
  :ensure t
  :config (progn
            (require 'spaceline)
            (require 'spaceline-config)
            (spaceline-spacemacs-theme)
            (spaceline-helm-mode)
            (setq spaceline-highlight-face-func #'spaceline-highlight-face-evil-state)))

(use-package golden-ratio
  :ensure t
  :config (progn
            (golden-ratio-mode)
            (setq golden-ratio-extra-commands
                  (append golden-ratio-extra-commands
                          '(evil-window-left
                            evil-window-right
                            evil-window-up
                            evil-window-down
                            buf-move-left
                            buf-move-right
                            buf-move-up
                            buf-move-down
                            window-number-select
                            select-window
                            select-window-1
                            select-window-2
                            select-window-3
                            select-window-4
                            select-window-5
                            select-window-6
                            select-window-7
                            select-window-8
                            select-window-9)))))

(use-package linum-relative
  :ensure t)
(use-package which-key
  :ensure t
  :config (progn
            (require 'which-key)
            (which-key-mode)))
(use-package all-the-icons
  :ensure t)
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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
