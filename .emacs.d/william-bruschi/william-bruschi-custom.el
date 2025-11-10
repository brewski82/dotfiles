(provide 'william-bruschi-custom)

(defun william-bruschi/eglot-current-server ()
  (interactive)
  (message "%s" (process-command (jsonrpc--process (eglot-current-server)))))

(defun william-bruschi/remove-delete-whitespace-hook ()
  (interactive)
  (remove-hook 'before-save-hook 'delete-trailing-whitespace t))

(defun william-bruschi/unfill-paragraph ()
  "Unfills a paragraph."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun william-bruschi/unfill-region ()
  "Unfill the selected region."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

(defun william-bruschi/swap-paren ()
  "Swap brackets and parens"
  (interactive)
  (keyboard-translate ?\( ?\[)
  (keyboard-translate ?\[ ?\()
  (keyboard-translate ?\) ?\])
  (keyboard-translate ?\] ?\)))

(william-bruschi/swap-paren)

(defun william-bruschi/toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value)
                                  'fullboth)))))

(defun william-bruschi/wn ()
  (interactive)
  (let ((word (current-word)))
    (shell-command (concat "wn " word " -over"))))

(defun william-bruschi/decapitalize-first-letter (&optional arg)
  "Forces the first letter of the word at point to be lowercase"
  (interactive)
  (let* ((word (or arg (current-word)))
         (lower-case-word (concat (downcase (string (string-to-char word)))
                                  (substring word 1))))
    (kill-new lower-case-word)))

(defun william-bruschi/capitalize-first-letter (word)
  (concat (upcase (substring word 0 1)) (substring word 1)))

(defun william-bruschi/column-name-to-label ()
  (interactive)
  (let* ((word (current-word))
         (word-list (split-string word "_"))
         (capitalized-word-list (mapcar (lambda (x)
                                          (concat (william-bruschi/capitalize-first-letter x)
                                                  " "))
                                        word-list))
         (label (apply #'concat capitalized-word-list)))
    (kill-new (substring label 0 (- (length label) 1)))))

(defun william-bruschi/column-name-to-field ()
  (interactive)
  (let* ((word (current-word))
         (capitalized-word (capitalize word))
         (capitalized-field (replace-regexp-in-string "_" "" capitalized-word))
         (field (william-bruschi/decapitalize-first-letter capitalized-field)))
    (kill-new field)))

(defun william-bruschi/field-name-to-sql-column ()
  (interactive)
  (setq case-fold-search nil)
  (let* ((camel-cased-word (current-word))
         (underscored-word (replace-regexp-in-string "\\([A-Z]\\)" "_\\1" camel-cased-word t))
         (field (downcase underscored-word)))
    (kill-new field))
  (setq case-fold-search t))

;;; Initialize workspace
(defun william-bruschi/acceptable-file-to-open (file)
  (or (string= (file-name-extension file) "java")
      (string= (file-name-extension file) "sql")
      (string= (file-name-extension file) "conf")
      (string= (file-name-extension file) "sh")
      (string= (file-name-extension file) "md")
      (string= (file-name-extension file) "markdown")
      (string= (file-name-extension file) "py")
      (string= (file-name-extension file) "properties")
      (string= (file-name-extension file) "yml")
      (string= (file-name-extension file) "yaml")
      (string= (file-name-extension file) "js")
      (string= (file-name-extension file) "html")
      (string= (file-name-extension file) "css")
      (string= (file-name-extension file) "scss")))

(defun william-bruschi/load-all-files-in-root-directory (directory)
  (mapc (lambda (item)
          (when (not (string= "." (cl-subseq (file-name-nondirectory item) 0 1)))
            (cond ((and (file-directory-p item) (not (file-symlink-p item)) (not (string= "target" (file-name-nondirectory item ))))
                   (william-bruschi/load-all-files-in-root-directory item))
                  ((and (not (file-directory-p item)) (william-bruschi/acceptable-file-to-open item))
                   (find-file-other-window item))
                  (t nil))))
        (directory-files directory 'absolute)))

(defun william-bruschi/uniquify-region-lines (beg end)
  "Remove duplicate adjacent lines in region."
  (interactive "*r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
      (replace-match "\\1"))))

(defun william-bruschi/uniquify-buffer-lines ()
  "Remove duplicate adjacent lines in the current buffer."
  (interactive)
  (william-bruschi/uniquify-region-lines (point-min) (point-max)))

;;; For SQL files I like to set tab spaces to 2
(defun william-bruschi/local-two-spaces-for-tabs-hook ()
  (setq tab-stop-list (number-sequence 2 120 2))
  (setq tab-width 2)
  (setq indent-tabs-mode nil))

(add-hook 'sql-mode-hook 'william-bruschi/local-two-spaces-for-tabs-hook)

;;; http://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/
(defun william-bruschi/copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))


(defun william-bruschi/copy-file-name-from-vc-root-to-clipboard ()
  "Copy the current buffer file name to the clipboard, excluding
the heirarchy outside the vc root."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (let ((filename-vc-root (file-relative-name filename (file-name-directory (directory-file-name (vc-root-dir))))))
        (kill-new filename-vc-root)
        (message "Copied buffer file name '%s' to the clipboard." filename-vc-root)))))

(defun william-bruschi/file-name-and-line-number ()
  "Get the current buffer file name and line number to the
clipboard. Also supports magit diff buffers."
  (interactive)
  (if (buffer-file-name)
      (format "%s:%d" (buffer-file-name) (line-number-at-pos))
    (let* ((filename (magit-file-at-point))
           (line-number (car (william-bruschi/magit-diff-get-line-number)))
           (file-and-line (format "%s:%d" filename line-number)))
      file-and-line)))

(defun william-bruschi/copy-file-name-and-line-number-to-clipboard ()
  "Copy the current buffer file name and line number to the clipboard."
  (interactive)
  (let ((result (william-bruschi/file-name-and-line-number)))
    (message "Copied '%s' to the clipboard." result)
    (kill-new result)))

(defun william-bruschi/run-current-python-file ()
  (interactive)
  (save-buffer)
  (let ((filename (buffer-file-name)))
    (compile (concat "python3 " filename))))

(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map (kbd "<f5>") 'william-bruschi/run-current-python-file)))

(defun william-bruschi/isend-block ()
  "Sends a block of text to a terminal. Use with isend."
  (interactive)
  (save-excursion
    (backward-paragraph)
    (set-mark-command nil)
    (forward-paragraph)
    (isend-send)))

(add-hook 'sh-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-c") 'william-bruschi/isend-block)))

(defun william-bruschi/magit-three-windows ()
  "Show three main windows for magit."
  (interactive)
  (magit-status)
  (delete-other-windows)
  (split-window-right)
  (magit-show-refs)
  (other-window 1)
  (split-window-right)
  (other-window 1)
  (magit-log-head)
  (other-window 2)
  (balance-windows))

(defun william-bruschi/vterm-buffer-name-suffix (directory)
  (file-name-nondirectory
   (directory-file-name (file-name-directory directory))))

(defun william-bruschi/vterm-open (directory)
  (let* ((vterm-buffer-name
          (concat "vterm-"
                  (william-bruschi/vterm-buffer-name-suffix directory)))
         (default-directory directory)
         (vterm-buffer (vterm t)))
    (rename-buffer vterm-buffer-name)
    vterm-buffer-name))

(defun william-bruschi/vterm-current-dir ()
  "Opens vterm in a new buffer with a new based on the current
directory."
  (interactive)
  (let* ((current-directory (or (buffer-file-name)
                                default-directory
                                (substitute-in-file-name "$HOME/"))))
    (william-bruschi/vterm-open current-directory)))

(defun william-bruschi/vterm-home ()
  "Opens vterm in a new buffer with the current directory set to the
user's home directory. Names the buffer vterm-home."
  (interactive)
  (let ((buffer-name "vterm-home"))
    (if (get-buffer buffer-name)
        (switch-to-buffer buffer-name)
      (let ((default-directory "~/"))
        (vterm buffer-name)))))

(defun william-bruschi/vterm-vc-root ()
  "Opens vterm in a new buffer in the root vc directory"
  (interactive)
  (let* ((current-directory (project-root (project-current))))
    (unless current-directory
      (error "vc root not found"))
    (william-bruschi/vterm-open current-directory)))

(defun william-bruschi/vterm-package ()
  "Opens vterm in a new buffer in the nearest package.json
directory."
  (interactive)
  (let* ((current-directory (jest-test-project-root (or (buffer-file-name)
                                                        default-directory))))
    (unless current-directory
      (error "package not found"))
    (william-bruschi/vterm-open current-directory)))

(defun william-bruschi/compile (command)
  "Runs compile with bash login shell"
  (interactive "scommand: ")
  (let ((comint-scroll-to-bottom-on-input t)
        (comint-scroll-to-bottom-on-output t)
        (comint-process-echoes t))
    ;; TODO: figure out how to prevent <RET> from re-sending the old input
    ;; See https://stackoverflow.com/questions/51275228/avoid-accidental-execution-in-comint-mode
    (compilation-start (concat "bash -i -c \"" command "\"") nil)))

(defun william-bruschi/magit-diff-get-line-number ()
  "Get the line number of the current diff hunk in Magit."
  (save-excursion
    (move-beginning-of-line 1)
    (let ((line-number 0)
          (line-count 0))
      (while (and (not (eobp)) (not (bobp)) (eq line-number 0))
        (when (looking-at "^@@ -[0-9]+,[0-9]+ \\+\\([0-9]+\\),\\([0-9]+\\) @@")
          (setq line-number (string-to-number (match-string 1))
                line-count (string-to-number (match-string 2))))
        (previous-line 1))
      (cons line-number line-count))))

(defun william-bruschi/open-in-intellij ()
  "Opens the current file in intellij. Requires the idea shell
script file to be on PATH."
  (interactive)
  (if (buffer-file-name)
      (start-process "emacs-open-in-intellij" nil
                     "idea" "--line" (number-to-string (line-number-at-pos))
                     (buffer-file-name))
    (start-process "emacs-open-in-intellij" nil
                   "idea" "--line" (number-to-string
                                    (car (william-bruschi/magit-diff-get-line-number)))
                   (magit-file-at-point))))
