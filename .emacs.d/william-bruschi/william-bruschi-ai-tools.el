(defvar-local william-bruschi/claude-code-buffer-name-local nil
  "Buffer-local variable to store the claude code buffer name.")

(defun william-bruschi/claude-code-buffer-name (vc-root-directory)
  "The name of the claude code buffer, based on the project's root directory.
Checks for a buffer-local variable first."
  (or william-bruschi/claude-code-buffer-name-local
      (concat "claude-" (william-bruschi/vterm-buffer-name-suffix vc-root-directory))))

(defun william-bruschi/set-claude-code-buffer-name ()
  "Interactive function to set the buffer-local claude code buffer
name from a list of buffers."
  (interactive)
  (let* ((buffers (mapcar 'buffer-name (buffer-list)))
         (selected-buffer (completing-read "Select buffer: " buffers)))
    (setq william-bruschi/claude-code-buffer-name-local selected-buffer)
    (message "Set claude code buffer name to: %s" selected-buffer)))

(defun william-bruschi/current-claude-code-buffer-name ()
  "Get the current claude code buffer name based on project root or
the buffer local variable."
  (or william-bruschi/claude-code-buffer-name-local
      (william-bruschi/claude-code-buffer-name (project-root (project-current)))))

(defun william-bruschi/claude-code-vterm ()
  "Opens or switches to a claude code session for the current project."
  (interactive)
  (let ((claude-buffer (william-bruschi/current-claude-code-buffer-name)))
    (if (get-buffer claude-buffer)
        (switch-to-buffer claude-buffer)  ; Switch to existing buffer
      (let* ((project-directory (project-root (project-current)))
             (default-directory project-directory)
             (buffer (vterm claude-buffer)))
        (with-current-buffer buffer
          (vterm-insert "claude")
          (vterm-send-return))))))

(defun william-bruschi/send-message-to-claude (message)
  "Send a MESSAGE to the current claude code session."
  (let ((buffer (william-bruschi/current-claude-code-buffer-name)))
    (display-buffer buffer)
    (with-current-buffer buffer
      (vterm-insert message)
      (vterm-send-return))))

(defun william-bruschi/send-to-claude ()
  "Sends a message to the current claude code session."
  (interactive)
  (let ((message (read-string "Enter message: ")))
    (william-bruschi/send-message-to-claude message)))

(defun william-bruschi/send-to-claude-with-buffer ()
  "Sends the context of the current buffer to claude along with a message."
  (interactive)
  (let* ((user-message (read-string "Enter message: "))
         (buffer (william-bruschi/current-claude-code-buffer-name))
         (buffer-contents (buffer-string))
         (message (concat buffer-contents "\n\n" user-message)))
    (william-bruschi/send-message-to-claude message)))

(defun william-bruschi/send-to-claude-with-region ()
  "Sends the selected region to claude along with a user-defined message."
  (interactive)
  (if (use-region-p)
      (let* ((user-message (read-string "Enter message: "))
             (region-text (buffer-substring-no-properties (region-beginning) (region-end)))
             (final-message (concat region-text "\n\n" user-message)))
        (william-bruschi/send-message-to-claude final-message))
    (message "No region selected.")))

(defun william-bruschi/send-message-to-claude-with-context ()
  "Can be called within project file buffers or magit buffers. Sends
the file name and line number to claude along with a user message."
  (interactive)
  (let* ((user-message (read-string "Enter message: "))
         (file (or (buffer-file-name)
                    (magit-file-at-point)))
         (line (if (buffer-file-name)
                   (line-number-at-pos)
                 (william-bruschi/magit-diff-get-line-number)))
         (final-message (concat "File name: " file "\n\n"
                                "line number: " (number-to-string line) "\n\n"
                                user-message)))
    (when file
      (william-bruschi/send-message-to-claude final-message))))

(transient-define-prefix william-bruschi/claude-trainsient ()
  "Claude Code Commands"
  [[ "Commands"
     ("s" "send message" william-bruschi/send-to-claude)
     ("b" "with buffer" william-bruschi/send-to-claude-with-buffer)
     ("r" "with region" william-bruschi/send-to-claude-with-region)
     ("c" "with context" william-bruschi/send-message-to-claude-with-context)]
   [ "Session"
     ("O" "open session" william-bruschi/claude-code-vterm)
     ("S" "select session" william-bruschi/set-claude-code-buffer-name)
     ("W" "in new worktree" (lambda ()
                              (interactive)
                              (call-interactively 'magit-worktree-branch)
                              (william-bruschi/claude-code-vterm)))]])

(defun william-bruschi/run-claude-code-menu ()
  (interactive)
  (william-bruschi/claude-trainsient))

(global-set-key (kbd "C-c c") 'william-bruschi/run-claude-code-menu)

(provide 'william-bruschi-ai-tools)

;;; For claude code prompt
(set-face-attribute 'nobreak-space nil :underline nil)
