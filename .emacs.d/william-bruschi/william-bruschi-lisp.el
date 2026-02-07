;;; Lisp setup
(use-package paredit
  :hook ((emacs-lisp-mode lisp-mode slime-repl-mode ) . paredit-mode))

(use-package sly
  :custom
  (inferior-lisp-program "sbcl")
  (sly-symbol-completion-mode nil))

(provide 'william-bruschi-lisp)
