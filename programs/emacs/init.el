;;; config

(savehist-mode 1)
(setq gc-cons-threshold 100000000)
;; yolo
(setq warning-minimum-level :emergency)
(keyboard-translate ?\C-x ?\C-a)
(keyboard-translate ?\C-a ?\C-x)

(require 'key-chord)
(key-chord-mode 1)

(setq evil-want-keybinding nil)
(require 'evil-search)
(evil-select-search-module 'evil-search-module 'evil-search)
(setq evil-want-C-i-jump nil)
(setq evil-want-C-u-scroll t)
(setq evil-want-integration t)

;; evil

;; need to require evil-leader and run (global-evil-leader-mode) before evil is loaded,
;; or otherwise it won’t work in buffers like *scratch* and *messages*.
(require 'evil-leader)
(global-evil-leader-mode)

(require 'evil)
(evil-mode 1)

(defun my/evil-delete (orig-fn beg end &optional type _ &rest args)
  (apply orig-fn beg end type ?_ args))
(advice-add 'evil-delete :around 'my/evil-delete)
(setq evil-kill-on-visual-paste nil)
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-visual-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-visual-state-map (kbd "k") 'evil-previous-visual-line)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-define evil-insert-state-map "jk" 'evil-execute-in-normal-state)
(define-key evil-normal-state-map (kbd "<escape>") 'evil-ex-nohighlight)
(require 'evil-collection)
(evil-collection-init)
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
  )
(require 'evil-surround)
(global-evil-surround-mode 1)

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
                 (kbd "C-f 0") 'eyebrowse-switch-to-window-config-0)

(require 'evil-easymotion)
(evilem-default-keybindings ",")

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

(require 'projectile)
(projectile-mode +1)
(setq projectile-completion-system 'ivy)

;; magit
(require 'magit)
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
                  '(display-buffer-same-window)))))

(evil-collection-init)
(evil-define-key 'normal git-rebase-mode-map
                 (kbd "K") 'git-rebase-move-line-up
                 (kbd "J") 'git-rebase-move-line-down
                 (kbd "0") 'evil-beginning-of-line)

(require 'evil-commentary)
(evil-commentary-mode)

;; org
(require 'evil-org)
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
(evil-org-agenda-set-keys)
(require 'org-bullets)
(add-hook 'org-mode-hook
          (lambda () (org-bullets-mode 1)))

;; ivy
(require 'ivy)
(defun my-ivy/alt-done ()
  (interactive)
  (ivy-alt-done t))
(ivy-mode 1)
(define-key ivy-minibuffer-map (kbd "C-t") 'my-ivy/alt-done)
(define-key ivy-minibuffer-map (kbd "C-n") 'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "C-p") 'ivy-previous-line)
(setq ivy-use-selectable-prompt t)
(setq ivy-on-del-error-function #'ignore)
(setq ivy-initial-inputs-alist ())
(setq ivy-re-builders-alist
      '((swiper . ivy--regex-plus)
        (t      . ivy--regex-fuzzy)))
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq browse-url-browser-function #'eww-browse-url)
(setq counsel-search-engine 'google)

(require 'flyspell-correct-ivy)
(setq flyspell-correct-interface #'flyspell-correct-ivy)

(require 'ag)
(setq ag-reuse-buffers t)
(setq ag-highlight-search t)

(require 'dumb-jump)
(setq dumb-jump-force-searcher 'ag)
(setq dumb-jump-selector 'ivy)
(dumb-jump-mode)
(add-hook 'prog-mode-hook
          (lambda ()
            (evil-define-key 'normal dumb-jump-mode-map
              (kbd "SPC g d") 'dumb-jump-go-prefer-external
              (kbd "SPC g l") 'dumb-jump-quick-look
              (kbd "SPC g D") 'dumb-jump-go-prefer-external-other-window)))

(require 'google-c-style)
(add-hook 'c-mode-common-hook
          (lambda()
            (subword-mode)
            (google-set-c-style)
            (google-make-newline-indent)
            (setq c-basic-offset 4)))


;; scala
(require 'scala-mode)
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))
(add-to-list 'auto-mode-alist '("\\.sbt\\'" . scala-mode))
(add-to-list 'interpreter-mode-alist '("scala" . scala-mode))
(require 'lsp-mode)
(add-hook 'scala-mode-hook
          (lambda()
            (lsp-deferred)
            (company-mode)))
(add-hook 'scala-mode-hook #'lsp-deferred)

(require 'dap-mode)
(dap-auto-configure-mode)
(add-hook 'lsp-mode-hook #'dap-mode)
(add-hook 'lsp-mode-hook #'dap-ui-mode)

(require 'sbt-mode)
;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
;; allows using SPACE when in the minibuffer
(substitute-key-definition
 'minibuffer-complete-word
 'self-insert-command
 minibuffer-local-completion-map)
;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
(setq sbt:program-options '("-Dsbt.supershell=false"))


;; java
(require 'lsp-java)
(add-hook 'java-mode-hook 'lsp)
(add-hook 'java-mode-hook
          (lambda ()
            (evil-define-key 'normal java-mode-map
              (kbd "K") 'lsp-describe-thing-at-point
              (kbd "g d") 'lsp-find-definition
              (kbd "C-c f r") 'lsp-find-references
              (kbd "C-c g o") 'lsp-java-generate-overrides
              (kbd "C-c b p") 'lsp-java-build-project
              (kbd "C-c e v") 'lsp-java-extract-to-local-variable
              (kbd "C-c e c") 'lsp-java-extract-to-constant
              (kbd "C-c r r") 'lsp-rename)
            (evil-define-key 'insert java-mode-map
              (kbd "TAB") 'completion-at-point)))


;; clj

;; This is useful for working with camel-case tokens, like names of
;; Java classes (e.g. JavaClassName)
(require 'clojure-mode)
(add-hook 'clojure-mode-hook 'subword-mode)

;;; cider
(require 'cider-mode)
;;; provides minibuffer documentation for the code you're typing into the repl
(add-hook 'cider-mode-hook 'eldoc-mode)
;;; don't go right to the REPL buffer when it's finished connecting
(setq cider-repl-pop-to-buffer-on-connect nil)
;;; When there's a cider error, show its buffer and switch to it
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)
;;; Where to store the cider history.
(setq cider-repl-history-file "~/.emacs.d/cider-history")
;;; Wrap when navigating history.
(setq cider-repl-wrap-history t)
;;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))
;;; custom
(defun my/clojure-hook (-mode-map)
  (when -mode-map
    (which-key-add-key-based-replacements (kbd "C-c l") "cider clear repl buffer.")
    (evil-define-key 'normal -mode-map
      (kbd "C-c l") '(lambda () (interactive) (cider-find-and-clear-repl-output t))
      (kbd "C-c r") 'cider-inspect-last-result
      (kbd "C-p") 'cider-repl-backward-input
      (kbd "C-n") 'cider-repl-forward-input)

    (evil-define-key 'insert -mode-map
      (kbd "TAB") 'cider-repl-tab))

  (modify-syntax-entry ?+ "w")
  (modify-syntax-entry ?- "w")
  (modify-syntax-entry ?_ "w")
  (modify-syntax-entry ?/ "w")
  (modify-syntax-entry ?: "w")
  (modify-syntax-entry ?> "w")
  (modify-syntax-entry ?< "w")
  (modify-syntax-entry ?? "w")
  (modify-syntax-entry ?! "w")
  (modify-syntax-entry ?. "w")
  (modify-syntax-entry ?* "w")
  )
(add-hook 'cider-clojure-interaction-mode-hook
          (lambda ()
            (evil-define-key 'insert cider-clojure-interaction-mode-map
              (kbd "S-RET") 'cider-eval-print-last-sexp)

            (evil-define-key 'normal cider-clojure-interaction-mode-map
              (kbd "S-RET") 'cider-eval-print-last-sexp)))
(add-hook 'cider-repl-mode-hook
          (lambda ()
            (paredit-mode t)
            (my/clojure-hook cider-repl-mode-map)
            (setq lsp-enable-indentation nil)
            (setq lsp-enable-completion-at-point nil)
            (my/clojure-hook cider-mode-map)))
(add-hook 'cider-browse-ns-mode-hook
          (lambda ()
            (evil-define-key 'normal cider-browse-ns-mode-map
              (kbd "RET") 'cider-browse-ns-operate-at-point
              (kbd "^") 'cider-browse-ns-all
              (kbd "d") 'cider-browse-ns-doc-at-point
              (kbd "q") 'cider-popup-buffer-quit-function
              (kbd "s") 'cider-browse-ns-find-at-point)))
(defun my/cider-insert-last-sexp-in-repl ()
  (interactive)
  (cider-insert-last-sexp-in-repl t))
(add-hook 'clojure-mode-hook
          (lambda ()
            (paredit-mode t)
            ;; keybindings
            (my/clojure-hook nil)
            (evil-define-key 'normal clojure-mode-map
              (kbd "C-c '") 'cider-jack-in
              (kbd "C-c b") 'cider-eval-buffer
              (kbd "C-c e") 'cider-eval-last-sexp
              (kbd "C-c v") 'cider-eval-sexp-at-point
              (kbd "C-c d n") 'cider-toggle-trace-ns
              (kbd "C-c d v") 'cider-toggle-trace-var
              (kbd "C-c n f") 'cider-browse-ns
              (kbd "C-c n r") 'cider-ns-refresh
              (kbd "C-c s n") 'cider-repl-set-ns
              (kbd "C-c s r") 'my/cider-insert-last-sexp-in-repl
              (kbd "> )") 'paredit-forward-slurp-sexp
              (kbd "< (") 'paredit-backward-slurp-sexp
              (kbd "< )") 'paredit-forward-barf-sexp
              (kbd "> (") 'paredit-backward-barf-sexp
              )
            ;;refactor
            (require 'clj-refactor)
            (clj-refactor-mode 1)
            ;; (yas-minor-mode 1) ; for adding require/use/import statements
            (cljr-add-keybindings-with-prefix "C-c C-m")
            ;; lsp
            (evil-define-key 'normal clojure-mode-map
              (kbd "C-c C-l r") 'lsp-find-references
              (kbd "C-c C-l d") 'lsp-find-definition)
            (lsp)))

;;; clj-refactor
(setq cljr-warn-on-eval nil)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(define-key company-active-map (kbd "RET") #'company-complete-selection) (define-key company-active-map (kbd "M-n") nil)
(define-key company-active-map (kbd "M-p") nil)
(define-key company-active-map (kbd "C-n") #'company-select-next)
(define-key company-active-map (kbd "C-p") #'company-select-previous)

(require 'flycheck)
(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
                (display-buffer-reuse-window
                  display-buffer-in-side-window)
                (side            . bottom)
                (reusable-frames . visible)
                (window-height   . 0.3)))
(global-flycheck-mode)
(require 'flycheck-popup-tip)
(add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode)
(require 'flycheck-checkbashisms)
(flycheck-checkbashisms-setup)

(require 'ranger)
(ranger-override-dired-mode t)
(setq ranger-parent-depth 2)

(require 'highlight-parentheses)
(global-highlight-parentheses-mode)

(load-theme 'gruvbox t)

(require 'spaceline)
(require 'spaceline-config)
(spaceline-toggle-minor-modes-off)
(spaceline-emacs-theme)
(setq spaceline-highlight-face-func #'spaceline-highlight-face-evil-state)

(require 'which-key)
(which-key-mode)
(setq which-key-idle-delay 1)
(setq which-key-idle-secondary-delay 0.05)

(require 'tmux-pane)
(tmux-pane-mode 1)
(setq tmux-pane-terminal-folder-fn #'projectile-project-root
      tmux-pane-horizontal-percent 25)
(evil-define-key 'normal tmux-pane-mode-map
                 (kbd "t h") 'tmux-pane-toggle-horizontal
                 (kbd "t v") 'tmux-pane-toggle-vertical
                 (kbd "t q") 'tmux-pane-close
                 (kbd "t r") 'tmux-pane-rerun)

;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))
;; Highlights matching parenthesis
(show-paren-mode 1)
;; Highlight current line
(global-hl-line-mode 1)
;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

(setq auto-save-default nil)

(require 'iso-transl)

;; use 2 spaces for tabs
(defun die-tabs ()
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

(progn
  (when (memq window-system '(mac ns))
    (setq interprogram-cut-function nil)
    (setq interprogram-paste-function nil))

  (defun my-pbcopy ()
    (interactive)
    (let ((deactivate-mark t))
      (call-process-region (point) (mark) "xclip" nil nil nil "-selection" "c")))

  (defun my-pbpaste ()
    (interactive)
    (call-process-region (point) (if mark-active (mark) (point)) "xsel" nil t nil "-b"))

  (defun my-pbcut ()
    (interactive)
    (pbcopy)
    (delete-region (region-beginning) (region-end)))

  (define-key evil-visual-state-map (kbd "SPC c y") 'my-pbcopy)
  (define-key evil-visual-state-map (kbd "SPC c p") 'my-pbpaste)
  (define-key evil-normal-state-map (kbd "SPC c p") 'my-pbpaste)
  (define-key evil-visual-state-map (kbd "SPC c c") 'my-pbcut)
  (define-key evil-normal-state-map (kbd "SPC c s") 'counsel-yank-pop)
  )

(defun my/remove-trailing-whitespace-and-save ()
  (interactive)
  (whitespace-cleanup)
  (save-buffer))
(global-unset-key (kbd "C-s"))
(define-key evil-normal-state-map (kbd "C-s") 'my/remove-trailing-whitespace-and-save)

(defun my-flyspell/clean-overlays ()
  "Remove all overlays displayed by flyspell."
  (interactive)
  (flyspell-mode-off)
  (flyspell-mode-on))

(define-key evil-normal-state-map (kbd "z s") 'flyspell-buffer)
(define-key evil-normal-state-map (kbd "z f") 'flyspell-correct-wrapper)
(define-key evil-normal-state-map (kbd "z c") 'my-flyspell/clean-overlays)

(setq ispell-list-command "--list")
(setq ispell-program-name "aspell")

(setq electric-indent-mode nil)

(setq standard-indent 2)

(setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAIT" "|" "DONE" "CANCELED")))

(setq org-agenda-files '("~/todos.org"))
(add-hook 'org-mode-hook
          (lambda ()
            (evil-define-key 'normal org-mode-map

              (kbd "-") 'org-ctrl-c-minus

              (kbd "C-t") nil
              (kbd "C-t c") 'org-table-create
              (kbd "C-t i c") 'org-table-insert-column
              (kbd "C-t i h") 'org-table-insert-hline
              (kbd "C-t k r") 'org-table-kill-row
              (kbd "C-t k c") 'org-table-delete-column
              (kbd "C-t m h") 'org-table-move-column-left
              (kbd "C-t m l") 'org-table-move-column-right
              (kbd "C-t m k") 'org-table-move-row-up
              (kbd "C-t m j") 'org-table-move-row-down
              (kbd "C-t a") 'org-table-align

              (kbd ";") 'org-cycle
              (kbd "\"") 'org-shifttab

              (kbd "g k") 'org-move-subtree-up
              (kbd "g j") 'org-move-subtree-down
              (kbd "K") nil
              (kbd "K") 'org-move-item-up
              (kbd "J") nil
              (kbd "J") 'org-move-item-down
              )
            ))
;; elisp editing
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

(setq vc-follow-symlinks t)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq default-directory "~")

(defun my/kill-buffers-magit ()
  "Restore previous window configuration and cleanup buffers."
  (interactive)
  (kill-matching-buffers "^magit*:*" :no-ask t))

(defun my/kill-buffers-with-prefix (prefix)
  (interactive "sPrefix name:")
  (kill-matching-buffers (format "\*%s*:*" prefix) :no-ask t))

(define-key evil-normal-state-map (kbd "SPC k b") 'my/kill-buffers-with-prefix)

(defun my/toggle-minor-modes (flag)
  (async-bytecomp-package-mode flag)
  (auto-composition-mode flag)
  (auto-encryption-mode flag)
  (comment-tags-mode flag)
  (company-mode flag)
  (diff-auto-refine-mode flag)
  (file-name-shadow-mode flag)
  (flycheck-mode flag)
  (flycheck-popup-tip-mode flag)
  (font-lock-mode flag)
  (highlight-parentheses-mode flag)
  (hs-minor-mode flag)
  (line-number-mode flag)
  (save-place-mode flag)
  (subword-mode flag)
  (tooltip-mode flag))

(defun my/disable-minor-modes ()
  (interactive)
  (my/toggle-minor-modes -1))

(defun my/enable-minor-modes ()
  (interactive)
  (my/toggle-minor-modes t))

(define-key evil-normal-state-map (kbd "SPC m d") 'my/disable-minor-modes)
(define-key evil-normal-state-map (kbd "SPC m e") 'my/enable-minor-modes)

(setq echo-keystrokes 0.01)

;; custom ibuffer
(eval-after-load 'ibuffer
  '(progn
     (evil-set-initial-state 'ibuffer-mode 'normal)
     (evil-define-key 'normal ibuffer-mode-map
       (kbd "0") 'digit-argument
       (kbd "1") 'digit-argument
       (kbd "2") 'digit-argument
       (kbd "3") 'digit-argument
       (kbd "4") 'digit-argument
       (kbd "5") 'digit-argument
       (kbd "6") 'digit-argument
       (kbd "7") 'digit-argument
       (kbd "8") 'digit-argument
       (kbd "9") 'digit-argument

       (kbd "j") 'evil-next-line
       (kbd "k") 'evil-previous-line
       (kbd "l") 'ibuffer-visit-buffer
       (kbd "v") 'ibuffer-toggle-marks

       (kbd "m") 'ibuffer-mark-forward
       (kbd "t") 'ibuffer-toggle-marks
       (kbd "u") 'ibuffer-unmark-forward
       (kbd "=") 'ibuffer-diff-with-file
       (kbd "M-g") 'ibuffer-jump-to-buffer
       (kbd "M-s a C-s") 'ibuffer-do-isearch
       (kbd "M-s a M-C-s") 'ibuffer-do-isearch-regexp
       (kbd "M-s a C-o") 'ibuffer-do-occur
       (kbd "DEL") 'ibuffer-unmark-backward
       (kbd "M-DEL") 'ibuffer-unmark-all
       (kbd "* *") 'ibuffer-unmark-all
       (kbd "* c") 'ibuffer-change-marks
       (kbd "U") 'ibuffer-unmark-all-marks
       (kbd "* M") 'ibuffer-mark-by-mode
       (kbd "* m") 'ibuffer-mark-modified-buffers
       (kbd "* u") 'ibuffer-mark-unsaved-buffers
       (kbd "* s") 'ibuffer-mark-special-buffers
       (kbd "* r") 'ibuffer-mark-read-only-buffers
       (kbd "* /") 'ibuffer-mark-dired-buffers
       (kbd "* e") 'ibuffer-mark-dissociated-buffers
       (kbd "* h") 'ibuffer-mark-help-buffers
       (kbd "* z") 'ibuffer-mark-compressed-file-buffers
       (kbd ".") 'ibuffer-mark-old-buffers

       (kbd "d") 'ibuffer-mark-for-delete
       (kbd "C-d") 'ibuffer-mark-for-delete-backwards
       (kbd "x") 'ibuffer-do-kill-on-deletion-marks

       ;; immediate operations
       "`" 'ibuffer-switch-format
       "-" 'ibuffer-add-to-tmp-hide
       "+" 'ibuffer-add-to-tmp-show
       "b" 'ibuffer-bury-buffer
       (kbd ",") 'ibuffer-toggle-sorting-mode
       (kbd "s i") 'ibuffer-invert-sorting
       (kbd "s a") 'ibuffer-do-sort-by-alphabetic
       (kbd "s v") 'ibuffer-do-sort-by-recency
       (kbd "s s") 'ibuffer-do-sort-by-size
       (kbd "s f") 'ibuffer-do-sort-by-filename/process
       (kbd "s m") 'ibuffer-do-sort-by-major-mode

       (kbd "/ RET") 'ibuffer-filter-by-mode
       (kbd "/ m") 'ibuffer-filter-by-used-mode
       (kbd "/ M") 'ibuffer-filter-by-derived-mode
       (kbd "/ n") 'ibuffer-filter-by-name
       (kbd "/ E") 'ibuffer-filter-by-process
       (kbd "/ *") 'ibuffer-filter-by-starred-name
       (kbd "/ f") 'ibuffer-filter-by-filename
       (kbd "/ b") 'ibuffer-filter-by-basename
       (kbd "/ .") 'ibuffer-filter-by-file-extension
       (kbd "/ <") 'ibuffer-filter-by-size-lt
       (kbd "/ >") 'ibuffer-filter-by-size-gt
       (kbd "/ i") 'ibuffer-filter-by-modified
       (kbd "/ v") 'ibuffer-filter-by-visiting-file
       (kbd "/ c") 'ibuffer-filter-by-content
       (kbd "/ e") 'ibuffer-filter-by-predicate

       (kbd "/ r") 'ibuffer-switch-to-saved-filters
       (kbd "/ a") 'ibuffer-add-saved-filters
       (kbd "/ x") 'ibuffer-delete-saved-filters
       (kbd "/ d") 'ibuffer-decompose-filter
       (kbd "/ s") 'ibuffer-save-filters
       (kbd "/ p") 'ibuffer-pop-filter
       (kbd "/ <up>") 'ibuffer-pop-filter
       (kbd "/ !") 'ibuffer-negate-filter
       (kbd "/ t") 'ibuffer-exchange-filters
       (kbd "/ TAB") 'ibuffer-exchange-filters
       (kbd "/ o") 'ibuffer-or-filter
       (kbd "/ |") 'ibuffer-or-filter
       (kbd "/ &") 'ibuffer-and-filter
       (kbd "/ g") 'ibuffer-filters-to-filter-group
       (kbd "/ P") 'ibuffer-pop-filter-group
       (kbd "/ S-<up>") 'ibuffer-pop-filter-group
       (kbd "/ D") 'ibuffer-decompose-filter-group
       (kbd "/ /") 'ibuffer-filter-disable

       (kbd "M-n") 'ibuffer-forward-filter-group
       "\t" 'ibuffer-forward-filter-group
       (kbd "M-p") 'ibuffer-backward-filter-group
       [backtab] 'ibuffer-backward-filter-group
       (kbd "M-j") 'ibuffer-jump-to-filter-group
       (kbd "C-k") 'ibuffer-kill-line
       (kbd "C-y") 'ibuffer-yank
       (kbd "/ S") 'ibuffer-save-filter-groups
       (kbd "/ R") 'ibuffer-switch-to-saved-filter-groups
       (kbd "/ X") 'ibuffer-delete-saved-filter-groups
       (kbd "/ \\") 'ibuffer-clear-filter-groups

       (kbd "% n") 'ibuffer-mark-by-name-regexp
       (kbd "% m") 'ibuffer-mark-by-mode-regexp
       (kbd "% f") 'ibuffer-mark-by-file-name-regexp
       (kbd "% g") 'ibuffer-mark-by-content-regexp
       (kbd "% L") 'ibuffer-mark-by-locked

       (kbd "C-t") 'ibuffer-visit-tags-table

       (kbd "|") 'ibuffer-do-shell-command-pipe
       (kbd "!") 'ibuffer-do-shell-command-file
       (kbd "~") 'ibuffer-do-toggle-modified
       ;; marked operations
       (kbd "A") 'ibuffer-do-view
       (kbd "D") 'ibuffer-do-delete
       (kbd "E") 'ibuffer-do-eval
       (kbd "F") 'ibuffer-do-shell-command-file
       (kbd "I") 'ibuffer-do-query-replace-regexp
       (kbd "H") 'ibuffer-do-view-other-frame
       (kbd "N") 'ibuffer-do-shell-command-pipe-replace
       (kbd "M") 'ibuffer-do-toggle-modified
       (kbd "O") 'ibuffer-do-occur
       (kbd "P") 'ibuffer-do-print
       (kbd "Q") 'ibuffer-do-query-replace
       (kbd "R") 'ibuffer-do-rename-uniquely
       (kbd "S") 'ibuffer-do-save
       (kbd "T") 'ibuffer-do-toggle-read-only
       (kbd "L") 'ibuffer-do-toggle-lock
       (kbd "r") 'ibuffer-do-replace-regexp
       (kbd "W") 'ibuffer-do-view-and-eval
       (kbd "X") 'ibuffer-do-shell-command-pipe

       (kbd "w") 'ibuffer-copy-filename-as-kill
       (kbd "B") 'ibuffer-copy-buffername-as-kill

       (kbd "RET") 'ibuffer-visit-buffer
       (kbd "e") 'ibuffer-visit-buffer
       (kbd "f") 'ibuffer-visit-buffer
       (kbd "C-x C-f") 'ibuffer-find-file
       (kbd "o") 'ibuffer-visit-buffer-other-window
       (kbd "C-o") 'ibuffer-visit-buffer-other-window-noselect
       (kbd "M-o") 'ibuffer-visit-buffer-1-window
       (kbd "C-x v") 'ibuffer-do-view-horizontally
       (kbd "C-c C-a") 'ibuffer-auto-mode
       (kbd "C-x 4 RET") 'ibuffer-visit-buffer-other-window
       (kbd "C-x 5 RET") 'ibuffer-visit-buffer-other-frame
       )))

(setq auto-window-vscroll nil)

(require 'flx)
(require 'flx-ido)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; Turn this behavior off because it's annoying
(setq ido-use-filename-at-point nil)

;; Don't try to match file across all "work" directories; only match files
;; in the current directory displayed in the minibuffer
(setq ido-auto-merge-work-directories-length -1)

;; Includes buffer names of recently open files, even if they're not
;; open now
(setq ido-use-virtual-buffers t)

;; projectile everywhere!
(projectile-global-mode)

(setq scroll-conservatively 200
      scroll-margin 3)

(progn
  (define-key evil-normal-state-map (kbd "C-w k") 'buf-move-up)
  (define-key evil-normal-state-map (kbd "C-w j") 'buf-move-down)
  (define-key evil-normal-state-map (kbd "C-w h") 'buf-move-left)
  (define-key evil-normal-state-map (kbd "C-w l") 'buf-move-right))

(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)

;; (progn
;;   (define-key evil-normal-state-map (kbd "TAB") 'switch-to-prev-buffer)
;;   (define-key evil-normal-state-map (kbd "SPC TAB") 'switch-to-next-buffer))

(progn
  (define-key evil-normal-state-map (kbd "C-m C-m") 'magit)
  (define-key evil-normal-state-map (kbd "C-m C-d") 'magit-dispatch)
  (define-key evil-normal-state-map (kbd "C-m C-f") 'magit-file-dispatch)
  )

(defun my/scroll-up ()
  (interactive)
  (evil-scroll-up nil)
  (evil-scroll-line-to-center (line-number-at-pos)))
(defun my/scroll-down ()
  (interactive)
  (evil-scroll-down nil)
  (evil-scroll-line-to-center (line-number-at-pos)))
(define-key evil-normal-state-map (kbd "C-b") 'my/scroll-up)
(define-key evil-normal-state-map (kbd "C-d") 'my/scroll-down)

(define-key evil-normal-state-map (kbd "/") 'swiper)

(menu-bar-mode -1) ; dont show menu bar
;(global-display-line-numbers-mode)

;; remove the graphical toolbar at the top
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(blink-cursor-mode 0)
(setq-default tab-width 2)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; no bell
(setq ring-bell-function 'ignore)

(set-face-attribute 'lazy-highlight nil :foreground "#262626" :background "#86dc2f")

(defun on-frame-open (frame)
  (if (not (display-graphic-p frame))
      (set-face-background 'default "unspecified-bg" frame)))
(on-frame-open (selected-frame))
(add-hook 'after-make-frame-functions 'on-frame-open)

(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(add-hook 'window-setup-hook 'on-after-init)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-=") 'text-scale-adjust)

(add-hook 'prog-mode-hook 'hs-minor-mode)

(require 'whitespace)
(setq whitespace-display-mappings
      '(
        (space-mark   ?\     [?·]      [?.])      ; space - middle dot
        (space-mark   ?\xA0  [?¤]     [?_])       ; hard space - currency sign
        (newline-mark ?\n    [?¶ ?\n]  [?¶ ?\n])	; eol - downwards arrow
        (tab-mark     ?\t    [? ?\t ] [? ?\t])	; tab - right guillemet
        ))

(define-key evil-normal-state-map (kbd "SPC t w") 'global-whitespace-mode)

;;; end
