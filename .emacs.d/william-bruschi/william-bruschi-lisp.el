;;; Lisp setup
(use-package paredit
  :hook ((emacs-lisp-mode lisp-mode slime-repl-mode) . paredit-mode))

(use-package sly
  :custom
  (inferior-lisp-program "sbcl"))

(add-hook 'lisp-mode-hook
          (lambda ()
            (company-mode 1)))

(add-hook 'sly-mrepl-mode-hook
          (lambda ()
            (company-mode 1)))

(provide 'william-bruschi-lisp)
