;; -*- lexical-binding: t; -*-
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

(use-package agent-shell
  :straight (:type git :host github :repo "xenodium/agent-shell")
  :bind (:map agent-shell-mode-map
              ("RET" . newline)
              ("M-RET" . shell-maker-submit))
  :config
  (setq agent-shell-file-completion-enabled t
        agent-shell-google-authentication (agent-shell-google-make-authentication
                                           :api-key (lambda () (get-gemini-password)))
        agent-shell-anthropic-claude-environment (agent-shell-make-environment-variables :inherit-env t))

  (define-prefix-command 'agent-shell-command-map)
  (define-key agent-shell-command-map (kbd "p") 'william-bruschi/agent-shell-send-prompt)
  (define-key agent-shell-command-map (kbd "c") 'william-bruschi/agent-shell-send-region-or-file-and-line-no-submit)
  (define-key agent-shell-command-map (kbd "i") 'william-bruschi/agent-shell-send-region-or-file-and-line)
  (define-key agent-shell-command-map (kbd "s") 'agent-shell)
  (define-key agent-shell-command-map (kbd "l") 'william-bruschi/agent-shell-list)
  (define-key agent-shell-command-map (kbd "t") 'william-bruschi/agent-shell-tile)
  (global-set-key (kbd "C-c A") agent-shell-command-map))

;;; Agent Shell Manager

(defcustom william-bruschi/agent-shell-ai-summaries nil
  "When non-nil, use AI to summarize completed agent tasks in the list."
  :type 'boolean
  :group 'william-bruschi)

(defcustom william-bruschi/agent-shell-summary-model nil
  "Model ID to use for summarizing agent shell output.
When nil, uses the agent buffer's current model."
  :type '(choice (const nil) string)
  :group 'william-bruschi)

(defvar william-bruschi/agent-shell-list-buffer-name "*agent-shell-list*")
(defvar william-bruschi/agent-shell--prev-states (make-hash-table :test #'eq))
(defvar-local william-bruschi/agent-shell-list--refresh-timer nil)
(defvar-local william-bruschi/agent-shell--summary nil)

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

(defun william-bruschi/agent-shell--status-string (buf &optional status)
  "Return propertized status string for BUF.
If STATUS is provided, use it instead of computing it."
  (let ((s (or status (william-bruschi/agent-shell--get-status buf))))
    (cond
     ((string= s "killed")      (propertize "Killed"     'face 'error))
     ((string= s "ready")       (propertize "Ready"      'face 'success))
     ((string= s "working")     (propertize "Working"    'face 'warning))
     ((string= s "waiting")     (propertize "Waiting"    'face 'font-lock-keyword-face))
     ((string= s "initializing")(propertize "Starting…"  'face 'font-lock-comment-face))
     (t                         (propertize "Unknown"    'face 'font-lock-comment-face)))))

(defun william-bruschi/agent-shell--get-last-content (buf lines)
  "Extract the last LINES from agent shell buffer BUF."
  (with-current-buffer buf
    (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
           (all-lines (split-string content "\n")))
      (mapconcat #'identity (last all-lines lines) "\n"))))

(defun william-bruschi/agent-shell--extract-json-text (json-lines)
  "Extract text content from JSON output lines."
  (let ((result nil))
    (dolist (line (split-string json-lines "\n" t))
      (condition-case nil
          (let* ((data (json-parse-string line))
                 (type (gethash "type" data)))
            (when (string= type "text")
              (let ((part (gethash "part" data)))
                (when part
                  (let ((text (gethash "text" part)))
                    (when text
                      (setq result (concat (or result "") text))))))))
        (error nil)))
    result))

(defun william-bruschi/agent-shell--request-summary (buf)
  "Request AI summary for agent shell BUF, store in buffer-local variable."
  (let* ((content (william-bruschi/agent-shell--get-last-content buf 80))
         (prompt (format "Summarize this agent's recent work in 5-10 words. Reply with only the summary, no extra text.\n\n%s" content))
         (api-buf (generate-new-buffer " *agent-shell-api*"))
         (default-directory temporary-file-directory))
    (make-process
     :name (concat "agent-shell-summarize-" (buffer-name buf))
     :buffer api-buf
     :command (william-bruschi/agent-shell--summary-command buf prompt)
     :sentinel (lambda (proc event)
                 (when (string-match-p "finished" event)
                   (let ((text (with-current-buffer (process-buffer proc)
                                 (william-bruschi/agent-shell--extract-json-text (buffer-string)))))
                     (when (and text (not (string-empty-p text)) (buffer-live-p buf))
                       (with-current-buffer buf
                         (setq-local william-bruschi/agent-shell--summary
                                     (truncate-string-to-width text 50)))))
                   (kill-buffer (process-buffer proc)))))))

(defun william-bruschi/agent-shell--summary-command (_buf prompt)
  "Return command list for summarizing using opencode with PROMPT."
  (let ((model william-bruschi/agent-shell-summary-model))
    (append '("opencode" "run" "--format" "json")
            (when model (list "-m" model))
            (list prompt))))

(defun william-bruschi/agent-shell--extract-repo (buf)
  "Extract repo name from agent shell buffer BUF."
  (let ((name (buffer-name buf)))
    (if (string-match " @ \\(.+\\)$" name)
        (match-string 1 name)
      name)))

(defun william-bruschi/agent-shell--extract-agent-type (buf)
  "Extract agent type from agent shell buffer BUF."
  (let ((name (buffer-name buf)))
    (if (string-match "^\\(.+?\\) Agent" name)
        (match-string 1 name)
      "Agent")))

(defun william-bruschi/agent-shell--extract-branch (buf)
  "Extract VC branch/revision from agent shell buffer BUF's context directory.
Returns nil if not in a VC-managed directory."
  (let* ((default-directory (or (buffer-local-value 'default-directory buf) default-directory)))
    (condition-case err
        (let ((backend (vc-responsible-backend default-directory)))
          (when backend
            (cond
             ((eq backend 'Git)
              (car (vc-git-branches)))
             ((eq backend 'Hg)
              (car (vc-hg-branches)))
             (t
              (vc-working-revision default-directory backend)))))
      (error nil))))

(defun william-bruschi/agent-shell--render-entries ()
  "Render agent-shell buffers in list buffer with multi-line format."
  (let* ((bufs (william-bruschi/agent-shell-get-buffers))
         (sorted-bufs (sort bufs (lambda (a b)
                                   (let ((sa (william-bruschi/agent-shell--get-status a))
                                         (sb (william-bruschi/agent-shell--get-status b)))
                                     (if (string= sa sb)
                                         (let ((ta (buffer-local-value 'buffer-display-time a))
                                               (tb (buffer-local-value 'buffer-display-time b)))
                                           (if (and ta tb)
                                               (time-less-p tb ta)
                                             (if ta t nil)))
                                       (and (string= sa "killed") (not (string= sb "killed")))))))))
    (with-current-buffer william-bruschi/agent-shell-list-buffer-name
      (let ((inhibit-read-only t)
            (saved-agent (get-text-property (point) 'agent-buf)))
        (erase-buffer)
        (dolist (buf sorted-bufs)
          (let* ((status (william-bruschi/agent-shell--get-status buf))
                 (repo (william-bruschi/agent-shell--extract-repo buf))
                 (branch (william-bruschi/agent-shell--extract-branch buf))
                 (agent-type (william-bruschi/agent-shell--extract-agent-type buf))
                 (summary (buffer-local-value 'william-bruschi/agent-shell--summary buf))
                 (entry-start (point)))
            (insert repo "\n")
            (when branch
              (insert "  " branch "\n"))
            (insert "  " agent-type "\n")
            (insert "  " (william-bruschi/agent-shell--status-string buf status) "\n")
            (when summary
              (insert "  " summary "\n"))
            (insert "\n")
            (put-text-property entry-start (point) 'agent-buf buf)))
        (goto-char (point-min))
        (when saved-agent
          (while (and (not (eobp))
                      (not (eq (get-text-property (point) 'agent-buf) saved-agent)))
            (forward-line))
          (when (eobp) (goto-char (point-min))))))))

(defun william-bruschi/agent-shell--get-current-buf ()
  "Get the agent buffer under cursor."
  (get-text-property (point) 'agent-buf))

(defun william-bruschi/agent-shell--next-entry ()
  "Move to next agent entry."
  (interactive)
  (let ((current-buf (william-bruschi/agent-shell--get-current-buf))
        (found nil))
    (forward-line)
    (while (and (not found) (not (eobp)))
      (let ((buf-here (get-text-property (point) 'agent-buf)))
        (if (and buf-here (not (eq current-buf buf-here)))
            (setq found t)
          (forward-line))))
    (unless found (goto-char (point-min)))))

(defun william-bruschi/agent-shell--prev-entry ()
  "Move to previous agent entry."
  (interactive)
  (let ((current-buf (william-bruschi/agent-shell--get-current-buf))
        (found nil))
    (forward-line -1)
    (while (and (not found) (not (bobp)))
      (let ((buf-here (get-text-property (point) 'agent-buf)))
        (if (and buf-here (not (eq current-buf buf-here)))
            (setq found t)
          (forward-line -1))))
    (unless found
      (goto-char (point-max))
      (forward-line -1)
      (while (not (get-text-property (point) 'agent-buf))
        (forward-line -1)))))

(define-derived-mode william-bruschi/agent-shell-list-mode special-mode "AgentShellList"
  "Mode for managing agent shell buffers."
  (setq truncate-lines nil)
  (visual-line-mode 1)
  (define-key william-bruschi/agent-shell-list-mode-map (kbd "RET") #'william-bruschi/agent-shell-list-select)
  (define-key william-bruschi/agent-shell-list-mode-map (kbd "o")   #'william-bruschi/agent-shell-list-select-other-window)
  (define-key william-bruschi/agent-shell-list-mode-map (kbd "n")   #'william-bruschi/agent-shell--next-entry)
  (define-key william-bruschi/agent-shell-list-mode-map (kbd "p")   #'william-bruschi/agent-shell--prev-entry)
  (define-key william-bruschi/agent-shell-list-mode-map (kbd "g")   #'william-bruschi/agent-shell-list-refresh)
  (define-key william-bruschi/agent-shell-list-mode-map (kbd "k")   #'william-bruschi/agent-shell-list-kill)
  (define-key william-bruschi/agent-shell-list-mode-map (kbd "q")   #'quit-window)
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
        (dolist (buf (william-bruschi/agent-shell-get-buffers))
          (let* ((current-status (william-bruschi/agent-shell--get-status buf))
                 (prev-status (gethash buf william-bruschi/agent-shell--prev-states)))
            (when (and william-bruschi/agent-shell-ai-summaries
                       (not (string= current-status "killed"))
                       (or (string= prev-status "working") (string= prev-status "waiting"))
                       (string= current-status "ready"))
              (william-bruschi/agent-shell--request-summary buf))
            (puthash buf current-status william-bruschi/agent-shell--prev-states)))
        (william-bruschi/agent-shell--render-entries)))))

(defun william-bruschi/agent-shell-list-select ()
  "Switch to selected agent-shell buffer, keeping the list window open."
  (interactive)
  (when-let* ((buf (william-bruschi/agent-shell--get-current-buf)))
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
  (when-let* ((buf (william-bruschi/agent-shell--get-current-buf)))
    (if (not (buffer-live-p buf))
        (user-error "Buffer no longer exists")
      (let* ((list-window (selected-window))
             (other (next-window list-window 'skip-minibuf 'visible)))
        (when (eq other list-window)
          (split-window-right))
        (set-window-buffer (next-window list-window 'skip-minibuf 'visible) buf)))))

(defun william-bruschi/agent-shell-list-kill ()
  "Kill the selected agent-shell buffer after confirmation."
  (interactive)
  (when-let* ((buf (william-bruschi/agent-shell--get-current-buf)))
    (if (not (buffer-live-p buf))
        (user-error "Buffer no longer exists")
      (when (y-or-n-p (format "Kill buffer %s? " (buffer-name buf)))
        (kill-buffer buf)))))

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


(provide 'william-bruschi-ai-tools)
