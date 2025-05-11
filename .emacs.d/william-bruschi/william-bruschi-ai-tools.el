(use-package claude-code-tools
  :straight (:type git :host github :repo "brewski82/claude-code-tools.el" :branch "main"
                   :files ("*.el" (:exclude ".gitignore")))
  :bind ("C-c c" . claude-code-tools-run-claude-code-menu)
  ;; :config
  ;; Optional configuration here
  )


(provide 'william-bruschi-ai-tools)

;; ;;; For claude code prompt
(set-face-attribute 'nobreak-space nil :underline nil)
