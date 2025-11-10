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

(use-package gptel
  :custom (gptel-org-branching-context t)
  :bind (("C-c g" . gptel-menu))
  :config
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n")
  (gptel-make-gemini "Gemini" :key 'get-gemini-password :stream t)
  (gptel-make-anthropic "Claude" :stream t :key 'get-anthropic-password))


(use-package shell-maker
  :ensure t)

(use-package mcp-hub
  :straight (:type git :host github :repo "lizqwerscott/mcp.el" :branch "master"
                   :files ("*.el" (:exclude ".gitignore"))))

(use-package acp
  :straight (:type git :host github :repo "xenodium/acp.el"))

(use-package agent-shell
  :straight (:type git :host github :repo "xenodium/agent-shell")
  :custom
  (agent-shell-file-completion-enabled t))

;;; Agent Shell custom commands


(require 'gptel-integrations)

;;; https://github.com/karthink/gptel/wiki/Tools-collection

(gptel-make-tool
 :function (lambda (url)
             (with-current-buffer (url-retrieve-synchronously url)
               (goto-char (point-min))
               (forward-paragraph)
               (let ((dom (libxml-parse-html-region (point) (point-max))))
                 (run-at-time 0 nil #'kill-buffer (current-buffer))
                 (with-temp-buffer
                   (shr-insert-document dom)
                   (buffer-substring-no-properties (point-min) (point-max))))))
 :name "read_url"
 :description "Fetch and read the contents of a URL"
 :args (list '(:name "url"
                     :type string
                     :description "The URL to read"))
 :category "web")

(gptel-make-tool
 :function (lambda (filepath)
             (with-temp-buffer
               (insert-file-contents (expand-file-name filepath))
               (buffer-string)))
 :name "read_file"
 :description "Read and display the contents of a file"
 :args (list '(:name "filepath"
                     :type string
                     :description "Path to the file to read. Supports relative paths and ~."))
 :category "filesystem")

(gptel-make-tool
 :function (lambda (directory)
             (mapconcat #'identity
                        (directory-files directory)
                        "\n"))
 :name "list_directory"
 :description "List the contents of a given directory"
 :args (list '(:name "directory"
                     :type string
                     :description "The path to the directory to list"))
 :category "filesystem")

(gptel-make-tool
 :function (lambda (parent name)
             (condition-case nil
                 (progn
                   (make-directory (expand-file-name name parent) t)
                   (format "Directory %s created/verified in %s" name parent))
               (error (format "Error creating directory %s in %s" name parent))))
 :name "make_directory"
 :description "Create a new directory with the given name in the specified parent directory"
 :args (list '(:name "parent"
                     :type string
                     :description "The parent directory where the new directory should be created, e.g. /tmp")
             '(:name "name"
                     :type string
                     :description "The name of the new directory to create, e.g. testdir"))
 :category "filesystem")

(gptel-make-tool
 :function (lambda (path filename content)
             (let ((full-path (expand-file-name filename path)))
               (with-temp-buffer
                 (insert content)
                 (write-file full-path))
               (format "Created file %s in %s" filename path)))
 :name "create_file"
 :description "Create a new file with the specified content"
 :args (list '(:name "path"
                     :type string
                     :description "The directory where to create the file")
             '(:name "filename"
                     :type string
                     :description "The name of the file to create")
             '(:name "content"
                     :type string
                     :description "The content to write to the file"))
 :category "filesystem")

(defun my-gptel--edit_file (file-path file-edits)
  "In FILE-PATH, apply FILE-EDITS with pattern matching and replacing."
  (if (and file-path (not (string= file-path "")) file-edits)
      (with-current-buffer (get-buffer-create "*edit-file*")
        (insert-file-contents (expand-file-name file-path))
        (let ((inhibit-read-only t)
              (case-fold-search nil)
              (file-name (expand-file-name file-path))
              (edit-success nil))
          ;; apply changes
          (dolist (file-edit (seq-into file-edits 'list))
            (when-let ((line-number (plist-get file-edit :line_number))
                       (old-string (plist-get file-edit :old_string))
                       (new-string (plist-get file-edit :new_string))
                       (is-valid-old-string (not (string= old-string ""))))
              (goto-char (point-min))
              (forward-line (1- line-number))
              (when (search-forward old-string nil t)
                (replace-match new-string t t)
                (setq edit-success t))))
          ;; return result to gptel
          (if edit-success
              (progn
                ;; show diffs
                (ediff-buffers (find-file-noselect file-name) (current-buffer))
                (format "Successfully edited %s" file-name))
            (format "Failed to edited %s" file-name))))
    (format "Failed to edited %s" file-path)))

(gptel-make-tool
 :function #'my-gptel--edit_file
 :name "edit_file"
 :description "Edit file with a list of edits, each edit contains a line-number,
a old-string and a new-string, new-string will replace the old-string at the specified line."
 :args (list '(:name "file-path"
                     :type string
                     :description "The full path of the file to edit")
             '(:name "file-edits"
                     :type array
                     :items (:type object
                                   :properties
                                   (:line_number
                                    (:type integer :description "The line number of the file where edit starts.")
                                    :old_string
                                    (:type string :description "The old-string to be replaced.")
                                    :new_string
                                    (:type string :description "The new-string to replace old-string.")))
                     :description "The list of edits to apply on the file"))
 :category "filesystem")

(gptel-make-tool
 :function (lambda (command &optional working_dir)
             (with-temp-message (format "Executing command: `%s`" command)
               (let ((default-directory (if (and working_dir (not (string= working_dir "")))
                                            (expand-file-name working_dir)
                                          default-directory)))
                 (shell-command-to-string command))))
 :name "run_command"
 :description "Executes a shell command and returns the output as a string. IMPORTANT: This tool allows execution of arbitrary code; user confirmation will be required before any command is run."
 :args (list
        '(:name "command"
                :type string
                :description "The complete shell command to execute.")
        '(:name "working_dir"
                :type string
                :description "Optional: The directory in which to run the command. Defaults to the current directory if not specified."))
 :category "command"
 :confirm t
 :include t)
(defun run_async_command (callback command)
  "Run COMMAND asynchronously and pass output to CALLBACK."
  (condition-case error
      (let ((buffer (generate-new-buffer " *async output*")))
        (with-temp-message (format "Running async command: %s" command)
          (async-shell-command command buffer nil))
        (let ((proc (get-buffer-process buffer)))
          (when proc
            (set-process-sentinel
             proc
             (lambda (process _event)
               (unless (process-live-p process)
                 (with-current-buffer (process-buffer process)
                   (let ((output (buffer-substring-no-properties (point-min) (point-max))))
                     (kill-buffer (current-buffer))
                     (funcall callback output)))))))))
    (t
     ;; Handle any kind of error
     (funcall callback (format "An error occurred: %s" error)))))

(gptel-make-tool
 :function #'run_async_command
 :name "run_async_command"
 :description "Run an async command."
 :args (list
        '(:name "command"
                :type "string"
                :description "Command to run."))
 :category "command"
 :async t
 :include t)

(gptel-make-tool
 :function (lambda (buffer)
             (unless (buffer-live-p (get-buffer buffer))
               (error "Error: buffer %s is not live." buffer))
             (with-current-buffer buffer
               (buffer-substring-no-properties (point-min) (point-max))))
 :name "read_buffer"
 :description "Return the contents of an Emacs buffer"
 :args (list '(:name "buffer"
                     :type string
                     :description "The name of the buffer whose contents are to be retrieved"))
 :category "emacs")

(gptel-make-tool
 :function (lambda (buffer text)
             (with-current-buffer (get-buffer-create buffer)
               (save-excursion
                 (goto-char (point-max))
                 (insert text)))
             (format "Appended text to buffer %s" buffer))
 :name "append_to_buffer"
 :description "Append text to an Emacs buffer. If the buffer does not exist, it will be created."
 :args (list '(:name "buffer"
                     :type string
                     :description "The name of the buffer to append text to.")
             '(:name "text"
                     :type string
                     :description "The text to append to the buffer."))
 :category "emacs")

(defun codel-edit-buffer (buffer-name old-string new-string)
  "In BUFFER-NAME, replace OLD-STRING with NEW-STRING."
  (with-current-buffer buffer-name
    (let ((case-fold-search nil))  ;; Case-sensitive search
      (save-excursion
        (goto-char (point-min))
        (let ((count 0))
          (while (search-forward old-string nil t)
            (setq count (1+ count)))
          (if (= count 0)
              (format "Error: Could not find text to replace in buffer %s" buffer-name)
            (if (> count 1)
                (format "Error: Found %d matches for the text to replace in buffer %s" count buffer-name)
              (goto-char (point-min))
              (search-forward old-string)
              (replace-match new-string t t)
              (format "Successfully edited buffer %s" buffer-name))))))))

(gptel-make-tool
 :name "EditBuffer"
 :function #'codel-edit-buffer
 :description "Edits Emacs buffers"
 :args '((:name "buffer_name"
                :type string
                :description "Name of the buffer to modify"
                :required t)
         (:name "old_string"
                :type string
                :description "Text to replace (must match exactly)"
                :required t)
         (:name "new_string"
                :type string
                :description "Text to replace old_string with"
                :required t))
 :category "edit")

(defun codel-replace-buffer (buffer-name content)
  "Completely replace contents of BUFFER-NAME with CONTENT."
  (with-current-buffer buffer-name
    (erase-buffer)
    (insert content)
    (format "Buffer replaced: %s" buffer-name)))

(gptel-make-tool
 :name "ReplaceBuffer"
 :function #'codel-replace-buffer
 :description "Completely overwrites buffer contents"
 :args '((:name "buffer_name"
                :type string
                :description "Name of the buffer to overwrite"
                :required t)
         (:name "content"
                :type string
                :description "Content to write to the buffer"
                :required t))
 :category "edit")


;;; claude code config
;; (use-package claude-code
;;   :straight (:type git :host github :repo "stevemolitor/claude-code.el" :branch "main"
;;                    :files ("*.el" (:exclude "images/*")))
;;   :bind-keymap
;;   ("C-c c" . claude-code-command-map) ;; or your preferred key
;;   :config
;;   (claude-code-mode)
;;   :custom
;;   (claude-code-terminal-backend 'vterm)
;;   (claude-code-optimize-window-resize nil))

;; (defun my-claude-notify (title message)
;;   "Display a macOS notification with sound."
;;   (call-process "osascript" nil nil nil
;;                 "-e" (format "display notification \"%s\" with title \"%s\" sound name \"Glass\""
;;                              message title)))

;; (setq claude-code-notification-function #'my-claude-notify)


(provide 'william-bruschi-ai-tools)

;; ;;; For claude code prompt
(set-face-attribute 'nobreak-space nil :underline nil)

(gptel-make-preset 'anki
  :description "Create Anki flash cards"
  :backend "ChatGPT"                     ;gptel backend or backend name
  :model 'gpt-4.1-mini
  :system "You will help me create flashcards for Anki. I will provide you a link to a website or file. You will read the content and produce flashcards in a format suitable for importing into Anki.

Some info regarding the format:

Use the pipe | character to separate the front and back card text.

Surround the front and back card text in quotes because this will allow using newlines. If the content needs to include quotes, use double quotes: \"\".

For math equations, use MathJax format because it is supported by Anki.

The flashcards should ask specific questions and have specific answers. I'm especially interested in learning new terminology. When learning new words, create two flashcards: one that presents the word and asks for the definition, and another that presents the definition and asks for the word.

Very important: for the back of the flashcard, aka the answer section, the beginning should start with the exact answer. Then after adding some newlines, provide extra, generous context about the question and answer. It's nice to be able to read extra about the question and answer without having to navigate away from the flashcard.

Very important: each flashcard question should begin with the prefix \"tag: \" sans the quotes. Generate the tag based on the article we are creating flashcards for. For example, if the article is about microservices authored by Netflix, prefix each question with \"netflix microservices: \". Keep the prefix short, as it is primarily for helping me search for flashcards.

Here is example output for three flashcards:

\"hello\"|\"this is
a two line answer\"
\"two\"|\"this is a one line field\"
\"this includes a | (pipe) and \"\" (quote)\"|\"another field\"

For newlines, don't print \\n characters, actually use newlines like in the two line answer above.

"
  :tools '("read_url")) ;gptel tools or tool names

(gptel-make-preset 'rewrite-text
  :description "Rewrite general text"
  :system "I will provide you text, and I want you to correct any spelling and grammatical mistakes. Feel free to restructure into simple, shorter, clearer, language.")

(defun william-bruschi/agent-shell-send-region-or-file-and-line ()
  "Send a message to the agent, with the active region or file and line number."
  (interactive)
  (let ((msg (read-string "Message: ")))
    (let ((prompt (if (use-region-p)
                      (format "%s\n\n%s" msg (buffer-substring-no-properties (region-beginning) (region-end)))
                    (format "%s @%s" msg (william-bruschi/file-name-and-line-number)))))
      (let* ((shell-buffer-name (seq-first (agent-shell-project-buffers)))
             (shell-buffer (if shell-buffer-name
                               (get-buffer shell-buffer-name)
                             (agent-shell-start :config (agent-shell-select-config)))))
        (with-current-buffer shell-buffer
          (goto-char (point-max))
          (insert prompt)
          (comint-send-input))
        (switch-to-buffer-other-window shell-buffer)))))

(define-key global-map (kbd "C-c c x") 'william-bruschi/agent-shell-send-region-or-file-and-line)
