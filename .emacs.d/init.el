;;; Straight
;;; https://github.com/raxod502/straight.el#getting-started
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(push "~/.emacs.d/william-bruschi" load-path)
(require 'william-bruschi-init-pre nil nil)

;;; Helm
(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x r b" . helm-filtered-bookmarks)
         ("C-x b" . helm-buffers-list)
         ("M-s o" . helm-occur))
  :config (helm-mode 1)
  :custom
  (helm-completion-style 'emacs)
  (helm-buffer-max-length 60))

;;; Helm buffer ordering per
;;; http://snowsyn.net/2018/10/21/buffer-ordering-with-helm/ and
;;; https://github.com/emacs-helm/helm/issues/1492
(defun nm-around-helm-buffers-sort-transformer (candidates source)
  candidates)

(advice-add 'helm-buffers-sort-transformer
            :override #'nm-around-helm-buffers-sort-transformer)

(require 'cl-lib)

;;; Org
(require 'william-bruschi-org-setup nil nil)

(use-package company-mode
  :config
  (setq company-idle-delay 0.0
        company-minimum-prefix-length 1)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)))

(use-package yasnippet
  :config (yas-global-mode 1))

(use-package json-mode
  :config (setq js-indent-level 2))
(use-package which-key
  :config (which-key-mode -1))

(use-package tree-sitter-langs
  :ensure t)

;;; https://vxlabs.com/2022/06/12/typescript-development-with-emacs-tree-sitter-and-lsp-in-2022/
(use-package typescript-mode
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx))
  :custom (typescript-indent-level 2))

(use-package rjsx-mode)
(use-package ts-comint)
(use-package yaml-mode)
(use-package jest-test-mode
  :hook ((rjsx-mode typescript-mode) . jest-test-mode)
  :custom
  (jest-test-command-string "yarn run %s jest %s %s"))
(use-package prettier)

;;; eglot config
(dolist (hook '(python-mode-hook rjsx-mode-hook typescript-mode-hook))
  (add-hook hook (lambda ()
                   (eglot-ensure)
                   (company-mode 1)
                   (define-key eglot-mode-map (kbd "M-.") 'xref-find-definitions)
                   (define-key eglot-mode-map (kbd "C-.") 'xref-find-definitions)
                   (define-key eglot-mode-map (kbd "C-,") 'xref-go-back))))

;;; Magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))
(use-package forge
  :after magit)

;;; eslint
(require 'compile-eslint)
(push 'eslint compilation-error-regexp-alist)

;;; avy https://github.com/abo-abo/avy
(use-package avy
  :bind (("M-'" . avy-goto-char-2)))

;;; Beacon mode - flashes line when you scroll.
(use-package beacon
  :config (beacon-mode 1)
  :custom
  (beacon-blink-duration 0.6)
  (beacon-blink-when-focused t)
  (beacon-color .2)
  (beacon-mode t)
  (beacon-size 120))

;;; Buffer settings
(require 'uniquify)
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")
(set-default 'truncate-lines nil)

;;; Enable visual feedback on selections
(setq transient-mark-mode t)
(show-paren-mode t)

;;; Auto revert files
(global-auto-revert-mode 1)

;;; Backup files
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "backups") t)))

;;; Column mode
(setq column-number-mode t)

;;; Y or no
(defalias 'yes-or-no-p 'y-or-n-p)

;;; Whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace nil nil)

;;; Trash can
(setq delete-by-moving-to-trash t)

;;; Dired
(require 'dired-x)
(setq-default dired-omit-files-p t) ; this is buffer-local variable
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$\\|\\.lx32fsl"))
(setq dired-listing-switches "-alh")

(use-package visual-fill-column
  :custom
  (visual-fill-column-center-text t)
  (visual-fill-column-enable-sensible-window-split t)
  (visual-fill-column-adjust-for-text-scale))

(use-package adaptive-wrap)


(dolist (hook '(text-mode-hook org-mode-hook))
  (add-hook hook (lambda ()
                   (flyspell-mode 1)
                   (auto-fill-mode 0)
                   (visual-line-mode 1)
                   (adaptive-wrap-prefix-mode t)
		   (local-set-key (kbd "C-'") 'other-window)
                   (local-set-key (kbd "M-p") 'org-mark-ring-goto))))

;;; Avoid mouse cursor
(if window-system
    (mouse-avoidance-mode 'cat-and-mouse))

;; Web browsing - Best for browing documentation.
;; (defun w3m-browse-url-other-window (url &optional newwin)
;;   (let ((w3m-pop-up-windows t))
;;     (if (one-window-p) (split-window nil nil t))
;;     (other-window 1)
;;     (w3m-browse-url url newwin)))
(setq browse-url-browser-function 'browse-url-default-browser)

;; Term
;; Set the frame's title. %b is the name of the buffer. %+ indicates
;; the state of the buffer: * if modified, % if read only, or -
;; otherwise. Two of them to emulate the mode line. %f for the file
;; name. Incredibly useful!
;; (setq frame-title-format "Emacs: %b %+%+ %f")
;; (setq frame-title-format '("Emacs @ " system-name ": %b %+%+ %f"))
(setq frame-title-format (concat invocation-name "@" system-name ": %b %+%+ %f"))

;;; vterm https://github.com/akermu/emacs-libvterm
(use-package vterm
  :config (setq vterm-max-scrollback 10000))

;;; Scroll bar
(use-package sml-modeline
  :config
  (sml-modeline-mode 1)
  (scroll-bar-mode 1)
  (set-scroll-bar-mode 'right))

;; default to unified diffs
(setq diff-switches "-u")

;; Copy paste between apps
(setq x-select-enable-clipboard t)

;; Disable toolbar mode
(tool-bar-mode 0)

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Refresh directories automatically upon visiting
(setq dired-auto-revert-buffer t)

;;; Preserve case when dynamically expanding
(setq dabbrev-case-replace nil)

;;; Bell
(setq visible-bell 1)

;;; mode line
;; (set-face-foreground 'mode-line "white")
;; (set-face-background 'mode-line "dark green")

;;; Cursor
(blink-cursor-mode 1)
(set-cursor-color "yellow")
(setq-default cursor-type t blink-cursor-blinks 200)

;;; Lisp setup
(use-package paredit
  :hook ((emacs-lisp-mode lisp-mode slime-repl-mode) . paredit-mode))

(when (boundp 'wb-load-slime)
  (add-to-list 'load-path wb-slime-load-path)
  (require 'slime-autoloads)
  (setq inferior-lisp-program wb-inferior-lisp-program)
  (require 'slime)
  (slime-setup '(slime-repl slime-fancy slime-asdf slime-xref-browser slime-indentation slime-mrepl))
  (when (boundp 'wb-hyperspec-root)
    (setq common-lisp-hyperspec-root wb-hyperspec-root))
  (add-hook 'lisp-mode-hook
	    (lambda ()
	      (define-key lisp-mode-map (kbd "C-;") 'slime-complete-symbol)))
  (global-set-key "\C-cs" 'slime-selector)
  ;; Remove highlighting of "errors", "check-" and other such forms.
  (font-lock-remove-keywords
   'lisp-mode
   '(("(\\(a\\(?:\\(?:bo\\|sse\\)rt\\)\\|c\\(?:error\\|heck-type\\)\\|error\\|signal\\|warn\\)\\_>"
      (1 font-lock-warning-face))))
  (font-lock-remove-keywords
   'lisp-mode slime-additional-font-lock-keywords))

;;; Tabs and indentation.
(setq-default indent-tabs-mode nil)
(setq tab-stop-list (number-sequence 4 120 4))

;;; SQL
(with-eval-after-load "sql"
  (add-to-list 'sql-postgres-login-params '(port :default 5432)))

;;; SQLI
(dolist (hook '(sql-interactive-mode-hook comint-mode))
  (add-hook hook
            (lambda ()
              (local-set-key (kbd "C-c C-l C-l") 'comint-dynamic-list-input-ring))))

;;; Python
(setq python-shell-interpreter "python3")
(setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "--simple-prompt -i")

;;; Shell env variables.
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package isend-mode
  :config (setq isend-forward-line nil))

(use-package markdown-mode)

;;; From https://www.emacswiki.org/emacs/SmoothScrolling
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed t) ;; accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;;; Javascript
(with-eval-after-load 'js
  (define-key js-mode-map (kbd "M-.") nil))
;;; Default to rjsx mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
;; (add-to-list 'auto-mode-alist '("\\.ts\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(setq sentence-end-double-space nil)

;;; IDO
;; Turning this off for now as it does not seem to work consistentyly
;; with helm.
;; (ido-mode 1)
;; (global-set-key (kbd "C-x C-f") #'ido-find-file)

;;; Mac setup
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'meta)

;;; My functions

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

(defun william-bruschi/vterm-open (directory)
  (let* ((buffer-name-suffix
          (file-name-nondirectory
           (directory-file-name (file-name-directory directory))))
         (vterm-buffer-name (concat "vterm-" buffer-name-suffix))
         (default-directory directory)
         (vterm-buffer (vterm t)))
    (rename-buffer vterm-buffer-name)))

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
  (let* ((current-directory (or (vc-root-dir))))
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
    (let ((line-number 0))
      (while (and (not (eobp)) (not (bobp)) (eq line-number 0))
        (when (looking-at "^@@ -[0-9]+,[0-9]+ \\+\\([0-9]+\\),[0-9]+ @@")
          (setq line-number (string-to-number (match-string 1))))
        (previous-line 1))
      line-number)))

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
                                    (william-bruschi/magit-diff-get-line-number))
                   (magit-file-at-point))))



;;; Recent files
(recentf-mode 1)

;;; Run command setup
(use-package run-command
  :custom
  (run-command-default-runner 'run-command-runner-compile)
  (run-command-recipes '(ignore run-command-recipe-package-json)))

;;; See https://github.com/bard/emacs-run-command/blob/master/examples/run-command-recipe-package-json.el
;;; Adjusted to search the package file for yarn commands.
(defun run-command-recipe-package-json--get-scripts (package-json-file)
  "Extract NPM scripts from `package-json-file'."
  (with-temp-buffer
    (insert-file-contents package-json-file)
    (when-let ((script-hash (gethash "scripts" (json-parse-buffer))))
      (let ((scripts '())
            (is-yarn-p (string-match-p "\"yarn\s" (buffer-string))))
        (maphash (lambda (key _value) (push key scripts)) script-hash)
        (list scripts is-yarn-p)))))

(defun run-command-recipe-package-json ()
  (when-let* ((project-dir
               (locate-dominating-file default-directory "package.json"))
              (scripts-and-is-yarn-p
               (run-command-recipe-package-json--get-scripts (concat project-dir "package.json")))
              (scripts (car scripts-and-is-yarn-p))
              (script-runner
               (if (or (cadr scripts-and-is-yarn-p)
                       (file-exists-p (concat project-dir "yarn.lock")))
                   "yarn"
                 "npm")))
    (mapcar (lambda (script)
              (list :command-name script
                    :command-line (concat script-runner " run " script)
                    :display script
                    :working-dir project-dir))
            scripts)))

(use-package super-save
  :ensure t
  :config
  (super-save-mode +1)
  :custom
  (super-save-auto-save-when-idle t)
  (super-save-idle-duration 10))

;;; https://github.com/akicho8/string-inflection
(use-package string-inflection)

;;; Center text
(use-package olivetti
  :custom
  (olivetti-body-width (* fill-column 2)))
;; (use-package centered-window
;;   :ensure t
;;   :config
;;   (centered-window-mode t)
;;   :custom
;;   (cwm-centered-window-width 250)
;;   (cwm-incremental-padding t)
;;   (cwm-incremental-padding-% 1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-scroll-output t)
 '(eglot-autoshutdown t)
 '(eglot-confirm-server-initiated-edits nil)
 '(eglot-events-buffer-size 0)
 '(eglot-sync-connect 0)
 '(fill-column 80)
 '(helm-M-x-show-short-doc t)
 '(helm-company-initialize-pattern-with-prefix t)
 '(helm-move-to-line-cycle-in-source nil)
 '(markdown-command william-bruschi/markdown-command)
 '(org-hide-emphasis-markers t)
 '(recentf-max-saved-items 500)
 '(safe-local-variable-values
   '((william-bruschi/prettier . "npm run prettier")
     (william-bruschi/prettier . "npx prettier --write")
     (william-bruschi/prettier . "npx prettier")
     (william-bruschi/eslint . "npx eslint"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; Global Key bindings
(global-set-key (kbd "C-'") 'other-window)
(global-set-key (kbd "C-;") 'completion-at-point)
(global-set-key [f7] 'william-bruschi/magit-three-windows)
(global-set-key [f8] 'toggle-frame-maximized)
(global-set-key [f11] 'menu-bar-mode)
(global-set-key [f12] 'recompile)


(use-package all-the-icons
  :if (display-graphic-p))

;; (use-package material-theme
;;   :ensure t
;;   :config
;;   (load-theme 'material t))

;;; https://github.com/xenodium/dotsies/blob/main/emacs/features/fe-ui.el
;; (set-face-attribute 'default nil
;;                     :stipple nil
;;                     :background "#212121"
;;                     :foreground "#eeffff"
;;                     :inverse-video nil
;;                     ;; :family "Menlo" ;; or Meslo if unavailable: https://github.com/andreberg/Meslo-Font
;;                     ;; :family "Hack" ;; brew tap homebrew/cask-fonts && brew cask install font-hack
;;                     :family "JetBrains Mono" ;; brew tap homebrew/cask-fonts && brew install --cask font-jetbrains-mono
;;                     ;; :family "mononoki" ;; https://madmalik.github.io/mononoki/ or sudo apt-get install fonts-mononoki
;;                     :box nil
;;                     :strike-through nil
;;                     :overline nil
;;                     :underline nil
;;                     :slant 'normal
;;                     :weight 'normal
;;                     :width 'normal
;;                     :foundry "nil")


;; (straight-use-package
;;  '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))

;; (require 'nano)
;; (nano-theme-set-dark)
;; (nano-theme)
;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold t      ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t)   ; if nil, italics is universally disabled
;;   (load-theme 'doom-vibrant t)
;;   ;; (load-theme 'doom-xcode t)
;;   ;; (load-theme 'doom-old-hope t)
;;   ;; (load-theme 'doom-oceanic-next t)
;;   ;; (load-theme 'doom-monokai-pro t)
;;   ;; (load-theme 'doom-monokai-spectrum t)

;;   (setq doom-monokai-pro-padded-modeline t)

;;   ;; (setq doom-vibrant-brighter-modeline t
;;   ;;       doom-vibrant-brighter-comments t)

;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)
;;   ;; Enable custom neotree theme (all-the-icons must be installed!)
;;   ;; (doom-themes-neotree-config)
;;   ;; or for treemacs users
;;   ;; (setq doom-themes-treemacs-theme "doom-colors")
;;   ;; (doom-themes-treemacs-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))

;; (use-package solaire-mode
;;   :config
;;   (solaire-global-mode +1))

(straight-use-package
  '(nano :type git :host github :repo "rougier/nano-emacs"))

(require 'nano)
(require 'nano-theme-dark)
(nano-toggle-theme)

(electric-pair-mode 1)

;;; Highlight the line your are on.
(global-hl-line-mode 1)
(set-face-background 'hl-line "gray15")
(set-face-background 'hl-line "gray25")
(set-face-background 'hl-line "gray30")
(set-face-background 'hl-line "black")

;;; Support color in compilation mode. See
;;; https://stackoverflow.com/a/71785402q
(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

;;; AI
(use-package gptel
  :custom (gptel-org-branching-context t)
  :bind (("C-c g" . gptel-menu))
  :config
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n"))


;;; Typescript and JS config
(require 'william-bruschi-javascript nil nil)

(require 'william-bruschi-init-post nil t)
