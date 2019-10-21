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

(setq echo-keystrokes 0.01)
