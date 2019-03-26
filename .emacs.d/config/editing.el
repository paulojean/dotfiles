;; Customizations relating to editing a buffer.

;; Key binding to use "hippie expand" for text autocompletion
;; http://www.emacswiki.org/emacs/HippieExpand
(global-set-key (kbd "C-;") 'hippie-expand)

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

;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

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

;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

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
      (call-process-region (point) (mark) "pbcopy")))

  (defun my-pbpaste ()
    (interactive)
    (call-process-region (point) (if mark-active (mark) (point)) "pbpaste" t t))

  (defun my-pbcut ()
    (interactive)
    (pbcopy)
    (delete-region (region-beginning) (region-end)))

  (require 'evil)

  (define-key evil-visual-state-map (kbd "SPC c y") 'my-pbcopy)
  (define-key evil-visual-state-map (kbd "SPC c p") 'my-pbpaste)
  (define-key evil-normal-state-map (kbd "SPC c p") 'my-pbpaste)
  (define-key evil-visual-state-map (kbd "SPC c c") 'my-pbcut))

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
