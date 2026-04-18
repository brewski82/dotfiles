(require 'auth-source)

;;; AI
(defun get-gemini-password ()
  (funcall (plist-get
            (car (auth-source-search :host "gemini.google" :service "apikey")) :secret)))

(defun get-openai-password ()
  (funcall (plist-get
            (car (auth-source-search :host "api.openai.com" :service "apikey")) :secret)))

(defun get-anthropic-password ()
  (funcall (plist-get
            (car (auth-source-search :host "anthropic" :service "apikey")) :secret)))

(use-package shell-maker
  :ensure t)

(use-package acp
  :straight (:type git :host github :repo "xenodium/acp.el"))

(use-package agent-shell
  :straight (:type git :host github :repo "xenodium/agent-shell")
  :bind (:map agent-shell-mode-map
              ("RET" . newline)
              ("M-RET" . shell-maker-submit))
  :config
  (setq agent-shell-file-completion-enabled t
        agent-shell-google-authentication (agent-shell-google-make-authentication
                                           :api-key (lambda () (get-gemini-password)))
        agent-shell-anthropic-claude-environment (agent-shell-make-environment-variables :inherit-env t)))


(define-prefix-command 'agent-shell-command-map)

;;; Agent Shell custom commands

(defun william-bruschi/agent-shell-send-region-or-file-and-line (&optional arg)
  "Send a message to the agent, with the active region or file and line number.
With a prefix arg, do not submit the message."
  (interactive "P")
  (let* ((msg (read-string "Message: "))
         (prompt (if (use-region-p)
                     (format "%s\n\n%s\n\n" msg
                             (agent-shell--context))
                   (format "%s @%s" msg (william-bruschi/file-name-and-line-number))))
         (submit (not arg)))
    (agent-shell-insert :text prompt :submit submit)))

(defun william-bruschi/agent-shell-send-prompt (&optional arg)
  "Sends a prompt to agent shell"
  (interactive "P")
  (let* ((prompt (read-string "Message: "))
         (submit (not arg)))
    (agent-shell-insert :text prompt :submit submit)))

(defun william-bruschi/agent-shell-send-region-or-file-and-line-no-submit (&optional arg)
  (interactive)
  (save-window-excursion
    (william-bruschi/agent-shell-send-region-or-file-and-line t)))

(transient-define-prefix william-bruschi/ai-trainsient ()
  "AI Shell Commands"
  [[ "Send"
     ("p" "prompt" william-bruschi/agent-shell-send-prompt)
     ("c" "comment" william-bruschi/agent-shell-send-region-or-file-and-line-no-submit)
     ("i" "send comment" william-bruschi/agent-shell-send-region-or-file-and-line)]
   [ "Visit"
     ("s" "shell" agent-shell)]])

(defun william-bruschi/run-ai-menu ()
  (interactive)
  (save-buffer)
  (william-bruschi/ai-trainsient))

(define-key global-map (kbd "C-c a") 'william-bruschi/run-ai-menu)

(provide 'william-bruschi-ai-tools)
