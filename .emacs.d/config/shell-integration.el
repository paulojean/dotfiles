;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))

(progn
  (add-to-list 'load-path "~/code/shutils.el")
  (require 'shutils-history-ivy)
  (shutils-history/start-auto-update))

(progn
  (require 'evil)

  (defun my-shell/clear ()
    (interactive)
    (let ((comint-buffer-maximum-size 0))
      (comint-truncate-buffer)))

  (define-key evil-insert-state-map (kbd "C-l") 'my-shell/clear))

(use-package bash-completion
  :ensure t
  :quelpa
  (bash-completion :repo "szermatt/emacs-bash-completion" :fetcher github))

(progn
  (require 'bash-completion)
  (bash-completion-setup)
  )

;;; shell-integration.el ends here
