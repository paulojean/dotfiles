(setq auto-window-vscroll nil)

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

(progn
  (define-key evil-normal-state-map (kbd "TAB") 'switch-to-prev-buffer)
  (define-key evil-normal-state-map (kbd "SPC TAB") 'switch-to-next-buffer))

(progn
  (define-key evil-normal-state-map (kbd "C-m C-m") 'magit)
  (define-key evil-normal-state-map (kbd "C-m C-d") 'magit-dispatch)
  (define-key evil-normal-state-map (kbd "C-m C-f") 'magit-file-dispatch)
)

(progn
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
  )

(define-key evil-normal-state-map (kbd "/") 'swiper)

;;; navigation.el ends here
