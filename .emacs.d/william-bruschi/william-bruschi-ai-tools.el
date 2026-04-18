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

;;; Agent Shell Manager

(defvar william-bruschi/agent-shell-list-buffer-name "*agent-shell-list*")
(defvar-local william-bruschi/agent-shell-list--refresh-timer nil)

(defun william-bruschi/agent-shell-get-buffers ()
  "Return list of live agent-shell buffers."
  (seq-filter #'buffer-live-p (agent-shell-buffers)))

(defun william-bruschi/agent-shell--get-status (buf)
  "Get status of agent-shell BUF: waiting, ready, working, killed, or unknown."
  (with-current-buffer buf
    (if (not (boundp 'agent-shell--state))
        "unknown"
      (let* ((state agent-shell--state)
             (acp-proc (map-nested-elt state '(:client :process)))
             (acp-alive (and acp-proc (processp acp-proc) (process-live-p acp-proc)
                             (memq (process-status acp-proc) '(run open listen connect stop))))
             (comint-proc (get-buffer-process (current-buffer)))
             (comint-alive (and comint-proc (processp comint-proc) (process-live-p comint-proc)
                                (memq (process-status comint-proc) '(run open listen connect stop))))
             (alive (and acp-alive comint-alive)))
        (cond
         ((or (not comint-proc) (and (processp comint-proc) (not comint-alive))) "killed")
         ((and (map-elt state :client)
               (or (not acp-proc) (and (processp acp-proc) (not acp-alive)))) "killed")
         ((and alive (map-elt state :tool-calls) (> (length (map-elt state :tool-calls)) 0))
          (if (seq-find (lambda (tc) (map-elt (cdr tc) :permission-request-id))
                        (map-elt state :tool-calls))
              "waiting" "working"))
         ((and alive (fboundp 'shell-maker-busy) (shell-maker-busy)) "working")
         ((and alive (map-nested-elt state '(:session :id))) "ready")
         ((not (map-elt state :initialized)) "initializing")
         (t "unknown"))))))

(defun william-bruschi/agent-shell--status-string (buf)
  "Return propertized status string for BUF."
  (let ((status (william-bruschi/agent-shell--get-status buf)))
    (cond
     ((string= status "killed")      (propertize "Killed"     'face 'error))
     ((string= status "ready")       (propertize "Ready"      'face 'success))
     ((string= status "working")     (propertize "Working"    'face 'warning))
     ((string= status "waiting")     (propertize "Waiting"    'face 'font-lock-keyword-face))
     ((string= status "initializing")(propertize "Starting…"  'face 'font-lock-comment-face))
     (t                              (propertize "Unknown"    'face 'font-lock-comment-face)))))

(defun william-bruschi/agent-shell--entries ()
  "Return tabulated-list entries for all agent-shell buffers."
  (let* ((bufs (william-bruschi/agent-shell-get-buffers))
         (entries (mapcar (lambda (buf)
                            (list buf
                                  (vector (buffer-name buf)
                                          (william-bruschi/agent-shell--status-string buf))))
                          bufs)))
    (sort entries
          (lambda (a b)
            (let ((sa (substring-no-properties (aref (cadr a) 1)))
                  (sb (substring-no-properties (aref (cadr b) 1))))
              (and (string= sa "Killed") (not (string= sb "Killed"))))))))

(define-derived-mode william-bruschi/agent-shell-list-mode tabulated-list-mode "AgentShellList"
  "Mode for managing agent shell buffers."
  (setq tabulated-list-format [("Buffer" 40 t) ("Status" 12 t)]
        tabulated-list-padding 2)
  (define-key william-bruschi/agent-shell-list-mode-map (kbd "RET") #'william-bruschi/agent-shell-list-select)
  (define-key william-bruschi/agent-shell-list-mode-map (kbd "o")   #'william-bruschi/agent-shell-list-select-other-window)
  (define-key william-bruschi/agent-shell-list-mode-map (kbd "g")   #'william-bruschi/agent-shell-list-refresh)
  (define-key william-bruschi/agent-shell-list-mode-map (kbd "q")   #'quit-window)
  (tabulated-list-init-header)
  (when william-bruschi/agent-shell-list--refresh-timer
    (cancel-timer william-bruschi/agent-shell-list--refresh-timer))
  (setq william-bruschi/agent-shell-list--refresh-timer
        (run-with-timer 2 2 #'william-bruschi/agent-shell-list-refresh))
  (add-hook 'kill-buffer-hook
            (lambda ()
              (when william-bruschi/agent-shell-list--refresh-timer
                (cancel-timer william-bruschi/agent-shell-list--refresh-timer)
                (setq william-bruschi/agent-shell-list--refresh-timer nil)))
            nil t))

(defun william-bruschi/agent-shell-list-refresh ()
  "Refresh the agent-shell list buffer."
  (interactive)
  (let ((list-buf (get-buffer william-bruschi/agent-shell-list-buffer-name)))
    (when (and list-buf (buffer-live-p list-buf))
      (with-current-buffer list-buf
        (setq tabulated-list-entries (william-bruschi/agent-shell--entries))
        (tabulated-list-print t)))))

(defun william-bruschi/agent-shell-list-select ()
  "Switch to selected agent-shell buffer, keeping the list window open."
  (interactive)
  (when-let* ((buf (tabulated-list-get-id)))
    (if (not (buffer-live-p buf))
        (user-error "Buffer no longer exists")
      (let ((buf-window (get-buffer-window buf t))
            (target-window nil))
        (cond
         (buf-window (select-window buf-window))
         (t
          (walk-windows
           (lambda (win)
             (when (and (not target-window)
                        (not (eq win (selected-window)))
                        (with-current-buffer (window-buffer win)
                          (derived-mode-p 'agent-shell-mode)))
               (setq target-window win)))
           nil t)
          (if target-window
              (progn (set-window-buffer target-window buf)
                     (select-window target-window))
            (agent-shell--display-buffer buf))))))))

(defun william-bruschi/agent-shell-list-select-other-window ()
  "Open selected agent-shell buffer in another window, like dired's \\[dired-find-file-other-window]."
  (interactive)
  (when-let* ((buf (tabulated-list-get-id)))
    (if (not (buffer-live-p buf))
        (user-error "Buffer no longer exists")
      (let* ((list-window (selected-window))
             (other (next-window list-window 'skip-minibuf 'visible)))
        (when (eq other list-window)
          (split-window-right))
        (set-window-buffer (next-window list-window 'skip-minibuf 'visible) buf)))))

(defun william-bruschi/agent-shell-list ()
  "Toggle the agent-shell list side window."
  (interactive)
  (let* ((list-buf (get-buffer-create william-bruschi/agent-shell-list-buffer-name))
         (win (get-buffer-window list-buf)))
    (if (and win (window-live-p win))
        (delete-window win)
      (let ((window (display-buffer-in-side-window
                     list-buf
                     '((side . left)
                       (slot . 0)
                       (window-width . 0.15)
                       (preserve-size . (t . nil))
                       (window-parameters . ((no-delete-other-windows . t)))))))
        (with-current-buffer list-buf
          (william-bruschi/agent-shell-list-mode)
          (william-bruschi/agent-shell-list-refresh))
        (set-window-dedicated-p window t)
        (select-window window)))))

(defun william-bruschi/agent-shell-tile ()
  "Display up to 4 most recent agent-shell buffers in a tiled layout."
  (interactive)
  (let* ((recent-bufs (seq-take (william-bruschi/agent-shell-get-buffers) 4)))
    (if (not recent-bufs)
        (message "No agent-shell buffers found")
      (delete-other-windows)
      (cond
        ((= (length recent-bufs) 1)
         (switch-to-buffer (car recent-bufs)))
        ((= (length recent-bufs) 2)
         (switch-to-buffer (car recent-bufs))
         (split-window-right)
         (other-window 1)
         (switch-to-buffer (cadr recent-bufs)))
        ((= (length recent-bufs) 3)
         (switch-to-buffer (car recent-bufs))
         (split-window-right)
         (other-window 1)
         (switch-to-buffer (cadr recent-bufs))
         (split-window-below)
         (other-window 1)
         (switch-to-buffer (caddr recent-bufs)))
        ((= (length recent-bufs) 4)
         (switch-to-buffer (car recent-bufs))
         (split-window-right)
         (other-window 1)
         (switch-to-buffer (cadr recent-bufs))
         (split-window-below)
         (other-window 1)
         (switch-to-buffer (caddr recent-bufs))
         (split-window-right)
         (other-window 1)
         (switch-to-buffer (cadddr recent-bufs)))))))

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

;;; Agent Shell Command Map Bindings

(define-key agent-shell-command-map (kbd "p") 'william-bruschi/agent-shell-send-prompt)
(define-key agent-shell-command-map (kbd "c") 'william-bruschi/agent-shell-send-region-or-file-and-line-no-submit)
(define-key agent-shell-command-map (kbd "i") 'william-bruschi/agent-shell-send-region-or-file-and-line)
(define-key agent-shell-command-map (kbd "s") 'agent-shell)
(define-key agent-shell-command-map (kbd "l") 'william-bruschi/agent-shell-list)
(define-key agent-shell-command-map (kbd "t") 'william-bruschi/agent-shell-tile)

(define-key global-map (kbd "C-c A") agent-shell-command-map)

(provide 'william-bruschi-ai-tools)
