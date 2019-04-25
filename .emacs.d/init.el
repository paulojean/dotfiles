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
 '(nix-indent-function (quote nix-indent-line) t)
 '(package-selected-packages
   (quote
    (spacemacs-theme ranger ido-completing-read+ haskell-mode flycheck evil-surround dash clojure-mode parseedn parseclj a spotify apropospriate-theme psc-ide magit company cider bash-completion quelpa-use-package quelpa frame-local ov flycheck-checkbashisms google-translate flyspell-correct-popup flyspell-correct-ivy flycheck-joker buffer-move neotree company-mode counsel ivy-posframe flycheck-posframe posframe evil-collection ivy treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil treemacs lua-mode psci feature-mode clomacs evil-magit evil-commentary which-key origami linum-relative nix-mode auto-complete ag paredit projectile evil-leader ## evil)))
 '(safe-local-variable-values
   (quote
    ((elisp-lint-indent-specs
      (if-let* . 2)
      (when-let* . 1)
      (let* . defun)
      (nrepl-dbind-response . 2)
      (cider-save-marker . 1)
      (cider-propertize-region . 1)
      (cider-map-repls . 1)
      (cider--jack-in . 1)
      (cider--make-result-overlay . 1)
      (insert-label . defun)
      (insert-align-label . defun)
      (insert-rect . defun)
      (cl-defun . 2)
      (with-parsed-tramp-file-name . 2)
      (thread-first . 1)
      (thread-last . 1))
     (checkdoc-package-keywords-flag)
     (eval progn
           (require
            (quote projectile))
           (puthash
            (projectile-project-root)
            "cask exec buttercup -L ." projectile-test-cmd-map))))))

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless (require 'quelpa nil t)
  (with-temp-buffer
    (url-insert-file-contents "https://framagit.org/steckerhalter/quelpa/raw/master/bootstrap.el")
    (eval-buffer)))

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://framagit.org/steckerhalter/quelpa-use-package.git"))
(require 'quelpa-use-package)
(setq use-package-ensure-function 'quelpa)

(use-package quelpa
  :ensure t)

;; evil sutffs
(use-package evil
  :ensure t
  :init (progn
          (require 'evil-search)
          (evil-select-search-module 'evil-search-module 'evil-search)

          (setq evil-want-integration t)
          (setq evil-want-keybinding nil)

          (define-key evil-normal-state-map (kbd "<escape>") 'evil-ex-nohighlight))
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
    (defun my/remove-trailing-whitespace-and-sabe ()
      (interactive)
      (whitespace-cleanup)
      (save-buffer))

    (evil-leader/set-leader "SPC")
    (evil-leader/set-key
      "'" 'linum-mode
      "," 'other-window
      ";" 'linum-relative-toggle
      "SPC" 'counsel-M-x
      "a" 'ag
      "b" 'counsel-switch-buffer
      "d d" 'neotree-projectile-action
      "d o" 'delete-other-windows
      "d p" 'pwd
      "e" 'pp-eval-last-sexp
      "h" 'split-window-below
      "n" 'ibuffer
      "l e" 'flycheck-list-errors
      "o c a" 'hs-hide-all
      "o n" 'hs-toggle-hiding
      "o o a" 'hs-show-all
      "p f" 'counsel-projectile-find-file-dwim
      "p p" 'counsel-projectile-switch-project
      "q" 'delete-window
      "q" 'delete-window
      "r" 'ranger
      "s" 'my/remove-trailing-whitespace-and-sabe
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
            (projectile-mode +1)
            (setq projectile-completion-system 'ivy)))

(use-package counsel-projectile
  :ensure t
  :after projectile ivy)

(use-package magit
  :quelpa (magit :repo "magit/magit" :fetcher github)
  :ensure t
  :config (progn
            (setq magit-display-buffer-function
                  (lambda (buffer)
                    (display-buffer
                     buffer (if (and (derived-mode-p 'magit-mode)
                                     (memq (with-current-buffer buffer major-mode)
                                           '(magit-process-mode
                                             magit-revision-mode
                                             magit-diff-mode
                                             magit-stash-mode
                                             magit-status-mode)))
                                nil
                              '(display-buffer-same-window)))))))

(use-package evil-magit
  :ensure t
  :after magit
  :init (progn
           (setq evil-magit-want-horizontal-movement t)))

(if (eq system-type 'darwin)
    (use-package exec-path-from-shell
      :ensure t))

(use-package ivy
  :ensure t
  :config (progn
            (ivy-mode 1)
            (define-key ivy-minibuffer-map (kbd "C-h") 'ivy-alt-done)
            (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
            (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
            (setq ivy-on-del-error-function #'ignore)
            (setq ivy-initial-inputs-alist ())
            (setq ivy-re-builders-alist
                  '((swiper . ivy--regex-plus)
                    (t      . ivy--regex-fuzzy)))
            (setq ivy-use-virtual-buffers t)
            (setq enable-recursive-minibuffers t)))

(use-package flyspell-correct-ivy
  :ensure t
  :init (setq flyspell-correct-interface #'flyspell-correct-ivy))

(use-package evil-commentary
  :ensure t
  :config (evil-commentary-mode))

(use-package dash
  :ensure t)
(use-package s
  :ensure t)
(use-package ag
  :ensure t
  :config (progn
            (setq ag-highlight-search t)))

(use-package paredit
  :ensure t)
(use-package clojure-mode
  :ensure t)
(use-package clojure-mode-extra-font-locking
  :ensure t)
(use-package queue
  :ensure t)
(use-package cider
  :after queue
  :ensure t)
(use-package clomacs
  :ensure t)
(use-package yaml-mode
  :ensure t)
(use-package go-mode
  :ensure t)
(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'"
  :custom (nix-indent-function #'nix-indent-line))
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

(use-package company
  :ensure t
  :config (progn
            (add-hook 'after-init-hook 'global-company-mode)))

(use-package posframe
  :ensure t)
(use-package ivy-posframe
  :ensure t
  :after ivy
  :config (progn
            (push '(counsel-M-x . ivy-posframe-display-at-window-bottom-left) ivy-display-functions-alist)
            (push '(complete-symbol . ivy-posframe-display-at-point) ivy-display-functions-alist)
            (push '(swiper . ivy-posframe-display-at-window-bottom-left) ivy-display-functions-alist)
            (ivy-posframe-enable)))

(use-package company-posframe
  :ensure t
  :after company posframe
  :config (progn
            (company-posframe-mode 1)))

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

(use-package flycheck-popup-tip
  :ensure t
  :after flycheck
  :config (progn
            (add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode)))

(use-package flycheck-joker
  :ensure t
  :after flycheck)

(use-package flycheck-checkbashisms
  :ensure t
  :after flycheck
  :config
  (flycheck-checkbashisms-setup))

(use-package ido-completing-read+
  :ensure t)
(use-package rainbow-delimiters
  :ensure t)
(use-package tagedit
  :ensure t)
(use-package ranger
  :ensure t
  :config (progn
            (ranger-override-dired-mode t)
            (setq ranger-parent-depth 3)))

(use-package neotree
  :ensure t)

(use-package buffer-move
  :ensure t)

(use-package spacemacs-theme
  :ensure t)
(use-package spaceline
  :ensure t
  :config (progn
            (require 'spaceline)
            (require 'spaceline-config)
            (spaceline-toggle-minor-modes-off)
            (spaceline-emacs-theme)
            (setq spaceline-highlight-face-func #'spaceline-highlight-face-evil-state)))

(use-package linum-relative
  :ensure t)

(use-package which-key
  :ensure t
  :config (progn
            (require 'which-key)
            (which-key-mode)))

(use-package all-the-icons
  :ensure t
  :config (progn
            (setq neo-theme (if (display-graphic-p) 'icons 'arrow))))

(use-package all-the-icons-dired
  :ensure t
  :diminish all-the-icons-dired-mode
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package google-translate
  :ensure t
  :init (progn
          (require 'google-translate)
          (require 'google-translate-default-ui)
          (global-set-key "\C-ct" 'google-translate-at-point)
          (global-set-key "\C-cT" 'google-translate-query-translate)))

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
