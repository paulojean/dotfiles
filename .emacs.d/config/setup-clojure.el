;;;;
;; Clojure
;;;;

;; Enable paredit for Clojure
(add-hook 'clojure-mode-hook 'enable-paredit-mode)

;; This is useful for working with camel-case tokens, like names of
;; Java classes (e.g. JavaClassName)
(add-hook 'clojure-mode-hook 'subword-mode)

;; A little more syntax highlighting
(require 'clojure-mode-extra-font-locking)

;; syntax hilighting for midje
(add-hook 'clojure-mode-hook
          (lambda ()
            (setq inferior-lisp-program "lein repl")
            (font-lock-add-keywords
             nil
             '(("(\\(facts?\\)"
                (1 font-lock-keyword-face))
               ("(\\(background?\\)"
                (1 font-lock-keyword-face))))
            (define-clojure-indent (fact 1))
            (define-clojure-indent (facts 1))
            (rainbow-delimiters-mode)))

;;;;
;; Cider
;;;;

;; provides minibuffer documentation for the code you're typing into the repl
(add-hook 'cider-mode-hook 'eldoc-mode)

;; don't go right to the REPL buffer when it's finished connecting
(setq cider-repl-pop-to-buffer-on-connect nil)

;; When there's a cider error, show its buffer and switch to it
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)

;; Where to store the cider history.
(setq cider-repl-history-file "~/.emacs.d/cider-history")

;; Wrap when navigating history.
(setq cider-repl-wrap-history t)

;; enable paredit in your REPL
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

(setq cider-edit-jack-in-command t)

(add-hook 'cider-repl-mode
          (define-key cider-repl-mode-map (kbd "C-c l") 'cider-repl-clear-buffer))

(eval-after-load "evil-maps"
  (dolist (map '(evil-motion-state-map
                 evil-insert-state-map
                 evil-emacs-state-map))
    (define-key (eval map) "'" nil)))

(defun my/cider-insert-last-sexp-in-repl ()
  (interactive)
  (cider-insert-last-sexp-in-repl t))

(add-hook 'clojure-mode-hook
          (lambda ()
            (require 'evil)
            (define-key evil-normal-state-map (kbd "' '") 'cider-jack-in)
            (define-key evil-normal-state-map (kbd "' s s") 'cider-switch-to-repl-buffer)
            (define-key evil-normal-state-map (kbd "' s n") 'cider-repl-set-ns)
            (define-key evil-normal-state-map (kbd "' e") 'cider-eval-last-sexp)
            (define-key evil-normal-state-map (kbd "' r") 'my/cider-insert-last-sexp-in-repl)
            (define-key evil-normal-state-map (kbd "' b") 'cider-eval-buffer)
            (define-key evil-normal-state-map (kbd "' n r") 'cider-ns-refresh)

            (define-key evil-normal-state-map ">" nil)
            (define-key evil-normal-state-map "<" nil)
            (define-key evil-normal-state-map (kbd "> )") 'paredit-forward-slurp-sexp)
            (define-key evil-normal-state-map (kbd "< (") 'paredit-backward-slurp-sexp)
            (define-key evil-normal-state-map (kbd "< )") 'paredit-forward-barf-sexp)
            (define-key evil-normal-state-map (kbd "> (") 'paredit-backward-barf-sexp)

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
            ))
