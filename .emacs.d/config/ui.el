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

(provide 'ui)
;;; ui.el ends here
