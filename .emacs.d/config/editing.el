;; Customizations relating to editing a buffer.

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
