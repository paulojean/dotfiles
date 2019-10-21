;;;;
;; Clojure
;;;;

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

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

(setq cider-edit-jack-in-command t)

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
  )

(add-hook 'cider-clojure-interaction-mode-hook
          (lambda ()
            (evil-define-key 'insert cider-clojure-interaction-mode-map
              (kbd "S-RET") 'cider-eval-print-last-sexp)

            (evil-define-key 'normal cider-clojure-interaction-mode-map
              (kbd "S-RET") 'cider-eval-print-last-sexp)))

(add-hook 'cider-repl-mode-hook
          (lambda ()
            (paredit-mode)
            (my/clojure-hook cider-repl-mode-map)))

(add-hook 'cider-mode-hook
          (lambda ()
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
            (my/clojure-hook nil)
            (evil-define-key 'normal clojure-mode-map

              (kbd "C-c '") 'cider-jack-in
              (kbd "C-c s n") 'cider-repl-set-ns
              (kbd "C-c e") 'cider-eval-last-sexp
              (kbd "C-c r") 'my/cider-insert-last-sexp-in-repl
              (kbd "C-c b") 'cider-eval-buffer
              (kbd "C-c n f") 'cider-browse-ns
              (kbd "C-c n r") 'cider-ns-refresh
              (kbd "C-c d v") 'cider-toggle-trace-var
              (kbd "C-c d n") 'cider-toggle-trace-ns)
            ))
