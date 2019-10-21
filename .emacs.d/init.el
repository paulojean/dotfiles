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
    (typescript-mode lsp-python-ms lsp-haskell lsp-mode restclient dumb-jump anzu company-box font-lock+ pfuture ace-window f slack oauth2 websocket request circe emojify ht command-log-mode eyebrowse org-bullets forge ghub treepy closql emacsql-sqlite emacsql evil-org names comment-tags alert log4e gntp frog-jump-buffer avy clj-refactor hydra lv inflections edn peg multiple-cursors yasnippet dockerfile-mode groovy-mode key-chord spacemacs-theme ranger ido-completing-read+ haskell-mode flycheck evil-surround dash clojure-mode parseedn parseclj a spotify apropospriate-theme psc-ide magit company cider bash-completion quelpa-use-package quelpa frame-local ov flycheck-checkbashisms google-translate flyspell-correct-popup flyspell-correct-ivy flycheck-joker buffer-move neotree company-mode counsel ivy-posframe flycheck-posframe posframe evil-collection ivy treemacs-magit treemacs-projectile treemacs-evil treemacs lua-mode psci feature-mode clomacs evil-magit evil-commentary which-key origami linum-relative nix-mode auto-complete ag paredit projectile evil-leader ## evil)))
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

(use-package dash :ensure t)
(use-package s :ensure t)

;;; evil sutffs
;; next two imports are needed to make evil's import to work.
;; goto-chg is refering to a undo-tree version that does not exist.
;; It should be possible to remove them in the future.
(use-package undo-tree
  :ensure t)

(use-package goto-chg
  :ensure t)

(use-package key-chord
  :ensure t
  :config (key-chord-mode 1))
(use-package evil
  :ensure t
  :init (progn
          (require 'evil-search)
          (evil-select-search-module 'evil-search-module 'evil-search)

          (setq evil-want-C-i-jump nil)
          (setq evil-want-C-u-scroll t)
          (setq evil-want-integration t)
          (setq evil-want-keybinding nil))

  :config (progn
            (evil-mode 1)

            (defun my/evil-delete (orig-fn beg end &optional type _ &rest args)
              (apply orig-fn beg end type ?_ args))
            (advice-add 'evil-delete :around 'my/evil-delete)

            (define-key evil-insert-state-map (kbd "TAB") 'company-complete-common-or-cycle)
            (setq evil-kill-on-visual-paste nil)
            (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
            (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
            (define-key evil-visual-state-map (kbd "j") 'evil-next-visual-line)
            (define-key evil-visual-state-map (kbd "k") 'evil-previous-visual-line)
            (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
            (key-chord-define evil-insert-state-map "jk" 'evil-execute-in-normal-state)
            (define-key evil-normal-state-map (kbd "<escape>") 'evil-ex-nohighlight)))

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
      "SPC" 'counsel-M-x
      "a" 'ag
      "b" 'counsel-switch-buffer
      "d d" 'treemacs
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
      "u v" 'undo-tree-visualize
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

(use-package forge
  :ensure t
  :after magit)

(use-package evil-magit
  :ensure t
  :after magit
  :init (progn
           (setq evil-magit-want-horizontal-movement t)
           (evil-define-key 'normal git-rebase-mode-map
             (kbd "K") 'git-rebase-move-line-up
             (kbd "J") 'git-rebase-move-line-down
             (kbd "0") 'evil-beginning-of-line)))

(if (eq system-type 'darwin)
    (use-package exec-path-from-shell
      :ensure t))

(use-package evil-org
  :ensure t
  :after org
  :config
  (progn
    ;; Use syntax highlighting in source blocks while editing
    (setq org-src-fontify-natively t)

    ;; Record the time that a todo was archived
    (setq org-log-done 'time)

    (setq org-src-window-setup 'current-window)

    (add-hook 'org-mode-hook 'evil-org-mode)
    (add-hook 'evil-org-mode-hook
              (lambda ()
                (evil-org-set-key-theme)))
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys)))

(use-package org-bullets
  :ensure t
  :after org
  :config (progn
            (add-hook 'org-mode-hook
                      (lambda () (org-bullets-mode 1)))))

(use-package ivy
  :ensure t
  :config (progn
            (defun my-ivy/alt-done ()
              (interactive)
              (ivy-alt-done t))

            (ivy-mode 1)
            (define-key ivy-minibuffer-map (kbd "C-t") 'my-ivy/alt-done)
            (define-key ivy-minibuffer-map (kbd "C-n") 'ivy-next-line)
            (define-key ivy-minibuffer-map (kbd "C-p") 'ivy-previous-line)
            (setq ivy-on-del-error-function #'ignore)
            (setq ivy-initial-inputs-alist ())
            (setq ivy-re-builders-alist
                  '((swiper . ivy--regex-plus)
                    (t      . ivy--regex-fuzzy)))
            (setq ivy-use-virtual-buffers t)
            (setq enable-recursive-minibuffers t)
            (setq browse-url-browser-function #'eww-browse-url)
            (setq counsel-search-engine 'google)))

(use-package flyspell-correct-ivy
  :ensure t
  :init (setq flyspell-correct-interface #'flyspell-correct-ivy))

(use-package evil-commentary
  :ensure t
  :config (evil-commentary-mode))

(use-package ag
  :ensure t
  :config (progn
            (setq ag-reuse-buffers t)
            (setq ag-highlight-search t)))

(use-package dumb-jump
  :ensure t
  :init
  (setq dumb-jump-force-searcher 'ag)
  (setq dumb-jump-selector 'ivy)
  (dumb-jump-mode)
  (add-hook 'prog-mode-hook
            (lambda ()
              (evil-define-key 'normal dumb-jump-mode-map
                (kbd "SPC g d") 'dumb-jump-go-prefer-external
                (kbd "SPC g l") 'dumb-jump-quick-look
                (kbd "SPC g D") 'dumb-jump-go-prefer-external-other-window))))

(use-package paredit
  :ensure t
  :after evil
  :config (progn
            (add-hook 'prog-mode-hook #'enable-paredit-mode)
            (define-key evil-normal-state-map (kbd ">") nil)
            (define-key evil-normal-state-map (kbd "<") nil)
            (define-key evil-normal-state-map (kbd "> >") 'evil-shift-right-line)
            (define-key evil-normal-state-map (kbd "< <") 'evil-shift-left-line)
            (define-key evil-normal-state-map (kbd "> )") 'paredit-forward-slurp-sexp)
            (define-key evil-normal-state-map (kbd "< (") 'paredit-backward-slurp-sexp)
            (define-key evil-normal-state-map (kbd "> (") 'paredit-backward-barf-sexp)
            (define-key evil-normal-state-map (kbd "< )") 'paredit-forward-barf-sexp)))

(use-package lsp-mode :ensure t)

(use-package lsp-python-ms
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp))))
(use-package lsp-haskell
  :ensure t
  :after lsp-mode
  :hook
  (haskell-mode . (lambda ()
                    (require 'lsp-haskell)
                    (lsp))))

(use-package typescript-mode
  :ensure t
  :hook
  (typescript-mode . (lambda ()
                       (require 'typescript-mode)
                       (lsp)))
  (js-mode . (lambda ()
               (require 'typescript-mode)
               (lsp)))
  (javascript-mode . (lambda ()
                       (require 'typescript-mode)
                       (lsp))))

(use-package clojure-mode :ensure t)

(use-package clojure-mode-extra-font-locking :ensure t)

(use-package queue :ensure t)
(use-package cider
  :after queue
  :ensure t
  :config (add-to-list 'cider-test-defining-forms "defflow"))

(use-package clomacs :ensure t)
(use-package yaml-mode :ensure t)
(use-package go-mode :ensure t)
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
(use-package psci
  :ensure t
  :init (progn
          (add-hook 'purescript-mode-hook 'inferior-psci-mode)))

(use-package lua-mode :ensure t)

(use-package feature-mode
  :ensure t
  :config (progn
            (setq feature-default-language "en")))
(use-package markdown-mode :ensure t)

(use-package groovy-mode :ensure t)

(use-package dockerfile-mode :ensure t)

(use-package company
  :ensure t
  :config (progn
            (add-hook 'after-init-hook 'global-company-mode)
            (define-key company-active-map (kbd "RET") #'company-complete-selection)
            (define-key company-active-map (kbd "M-n") nil)
            (define-key company-active-map (kbd "M-p") nil)
            (define-key company-active-map (kbd "C-n") #'company-select-next)
            (define-key company-active-map (kbd "C-p") #'company-select-previous)))

(use-package company-box
  :ensure t
  :after company
  :hook (company-mode . company-box-mode))

(use-package comment-tags
  :ensure t
  :init (progn
          (autoload 'comment-tags-mode "comment-tags-mode")
          (setq comment-tags-keymap-prefix (kbd "C-c c"))
          (with-eval-after-load "comment-tags"
            (setq comment-tags-keyword-faces
                  `(("TODO" . ,(list :weight 'bold :foreground "#28ABE3"))
                    ("FIXME" . ,(list :weight 'bold :foreground "#DB3340"))
                    ("BUG" . ,(list :weight 'bold :foreground "#DB3340"))
                    ("HACK" . ,(list :weight 'bold :foreground "#E8B71A"))
                    ("INFO" . ,(list :weight 'bold :foreground "#F7EAC8"))
                    ("DONE" . ,(list :weight 'bold :foreground "#1FDA9A")))))
          (setq comment-tags-comment-start-only t
                comment-tags-require-colon nil
                comment-tags-case-sensitive nil
                comment-tags-show-faces t
                comment-tags-lighter nil)
          (add-hook 'prog-mode-hook 'comment-tags-mode)))

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

(use-package flx :ensure t)
(use-package flx-ido :ensure t)

(use-package ido-completing-read+ :ensure t)
(use-package rainbow-delimiters :ensure t)

(use-package tagedit :ensure t)
(use-package ranger
  :ensure t
  :config (progn
            (ranger-override-dired-mode t)
            (setq ranger-parent-depth 2)))

(use-package neotree :ensure t)

(use-package treemacs
  :ensure t
  :config
  (evil-define-key 'normal treemacs-mode-map
    (kbd "w a") 'treemacs-create-workspace
    (kbd "w d") 'treemacs-remove-workspace
    (kbd "w e") 'treemacs-edit-workspaces
    (kbd "w r") 'treemacs-rename-workspace
    (kbd "w s") 'treemacs-switch-workspace
    (kbd "p a") 'treemacs-add-project))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package buffer-move :ensure t)

(use-package spacemacs-theme :ensure t)
(use-package spaceline
  :ensure t
  :config (progn
            (require 'spaceline)
            (require 'spaceline-config)
            (spaceline-toggle-minor-modes-off)
            (spaceline-emacs-theme)
            (setq spaceline-highlight-face-func #'spaceline-highlight-face-evil-state)))

(use-package linum-relative :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1)
  (setq which-key-idle-secondary-delay 0.05))

(use-package command-log-mode :ensure t)

(use-package custom-tmux-pane
  :ensure t
  :quelpa (tmux-pane :repo "paulojean/emacs-tmux-pane" :fetcher github)
  :init (progn
          (require 'tmux-pane)
          (tmux-pane-mode 1)
          (setq tmux-pane-terminal-folder-fn #'projectile-project-root
                tmux-pane-horizontal-percent 25)
          (evil-define-key 'normal tmux-pane-mode-map
            (kbd "t h") 'tmux-pane-toggle-horizontal
            (kbd "t v") 'tmux-pane-toggle-vertical
            (kbd "t q") 'tmux-pane-close
            (kbd "t r") 'tmux-pane-rerun)))

(use-package eyebrowse
  :ensure t
  :after evil
  :config (progn
            (eyebrowse-mode t)
            (setq eyebrowse-wrap-around t)
            (setq eyebrowse-new-workspace 'projectile-switch-project)
            (define-key evil-normal-state-map (kbd "C-f") nil)
            (evil-define-key 'normal eyebrowse-mode-map
              (kbd "C-f c") 'eyebrowse-create-window-config
              (kbd "C-f l") 'eyebrowse-next-window-config
              (kbd "C-f h") 'eyebrowse-prev-window-config
              (kbd "C-f '") 'eyebrowse-last-window-config
              (kbd "C-f ,") 'eyebrowse-rename-window-config
              (kbd "C-f q") 'eyebrowse-close-window-config
              (kbd "C-f f") 'eyebrowse-switch-to-window-config
              (kbd "C-f 0") 'eyebrowse-switch-to-window-config-0)))

(use-package restclient
  :ensure t)

(use-package spotify.el
  :ensure t
  :quelpa (spotify :repo "danielfm/spotify.el" :fetcher github)
  :after evil
  :init
  (setq spotify-oauth2-client-secret (getenv "SPOTIFY_SECRET"))
  (setq spotify-oauth2-client-id (getenv "SPOTIFY_CLIENT_ID"))
  :config (progn
            (require 'spotify)
            (evil-define-key 'normal spotify-playlist-search-mode-map
              (kbd "q") 'kill-this-buffer
              (kbd "p") 'spotify-playlist-select
              (kbd "P") 'spotify-pause
              (kbd "J") 'spotify-playlist-load-more
              )
            (evil-define-key 'normal spotify-track-search-mode-map
              (kbd "q") 'kill-this-buffer
              (kbd "p") 'spotify-track-select
              (kbd "P") 'spotify-pause
              (kbd "J") 'spotify-track-load-more
              )
            ))

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
