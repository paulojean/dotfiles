;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.


;; "When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Turn on recent file mode so that you can more easily switch to
;; recently edited files when you first start emacs
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)

;;; those three lines are meant to disable auto-recentering
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

(require 'flx-ido)
(ido-mode t)
(ido-everywhere t)
(ido-ubiquitous-mode t)
;; disable ido faces to see flx highlights.
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

(setq helm-recentf-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t)
(setq helm-buffers-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t)
(setq helm-M-x-fuzzy-match t)
(setq helm-imenu-fuzzy-match t)
(setq helm-apropos-fuzzy-match t)
(setq helm-lisp-fuzzy-completion t)
(setq helm-session-fuzzy-match t)
(setq helm-etags-fuzzy-match t)

;; projectile everywhere!
(projectile-global-mode)

(add-to-list 'load-path "~/code/framecs")
(require 'framecs-ivy)
(require 'framecs)
(framecs/start-framecs)

(progn
  (global-unset-key (kbd "C-l"))
  (global-unset-key (kbd "C-h"))
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
  (define-key evil-normal-state-map (kbd "C-S-k") 'buf-move-up)
  (define-key evil-normal-state-map (kbd "C-S-j") 'buf-move-down)
  (define-key evil-normal-state-map (kbd "C-S-h") 'buf-move-left)
  (define-key evil-normal-state-map (kbd "C-S-l") 'buf-move-right)
  )

(define-key evil-normal-state-map (kbd "SPC f c") 'framecs/new-frame)
(define-key evil-normal-state-map (kbd "SPC f d") 'framecs/delete-frame)
(define-key evil-normal-state-map (kbd "SPC f w") 'framecs/new-workspace)
(define-key evil-normal-state-map (kbd "SPC f q") 'framecs/delete-current-workspace)
(define-key evil-normal-state-map (kbd "SPC f k") 'framecs/go-to-previous-workspace)
(define-key evil-normal-state-map (kbd "SPC f j") 'framecs/go-to-next-workspace)
(define-key evil-normal-state-map (kbd "SPC f h") 'framecs/go-to-previous-frame)
(define-key evil-normal-state-map (kbd "SPC f l") 'framecs/go-to-next-frame)
(define-key evil-normal-state-map (kbd "SPC f f l") 'framecs/select-frame-from-current-workspace)
(define-key evil-normal-state-map (kbd "SPC f f w") 'framecs/select-workspace)

(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)


(progn
  (define-key evil-normal-state-map "m" nil)
  (define-key evil-normal-state-map (kbd "m m") 'magit)
  (define-key evil-normal-state-map (kbd "m f") 'magit-file-popup)
  )

;;; navigation.el ends here
