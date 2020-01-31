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
