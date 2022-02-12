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

;;; Helm
(straight-use-package 'helm)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
;; (global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)

(require 'cl-lib)

;;; Org
(push "~/.emacs.d/william-bruschi" load-path)
(require 'william-bruschi-org-setup nil nil)

;;; LSP Mode
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'flycheck)
(straight-use-package 'company-mode)
(straight-use-package 'yasnippet)
(straight-use-package 'lsp-treemacs)
(straight-use-package 'helm-lsp)
(straight-use-package 'dap-mode)
(straight-use-package 'json-mode)
(straight-use-package 'which-key)
(straight-use-package 'typescript-mode)
(straight-use-package 'rjsx-mode)
(straight-use-package 'ts-comint)
(yas-global-mode 1)
(which-key-mode)
(add-hook 'python-mode-hook #'lsp)
;(add-hook 'sh-mode-hook #'lsp)
(add-hook 'rjsx-mode-hook #'lsp)
(add-hook 'rjsx-mode-hook #'electric-pair-local-mode)
(add-hook 'typescript-mode-hook #'lsp)
(add-hook 'typescript-mode-hook #'electric-pair-local-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)
;(setq lsp-diagnostics-provider :none)
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      create-lockfiles nil) ;; lock files will kill `npm start'
(global-set-key (kbd "C-;") 'completion-at-point)
(with-eval-after-load 'lsp-mode
  (require 'dap-chrome)
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))
(lsp-treemacs-sync-mode 1)


;;; Theme
;; (straight-use-package 'soft-charcoal-theme)
;; (load-theme 'soft-charcoal t)
;; (straight-use-package 'spacemacs-theme)
;; (load-theme 'spacemacs-dark t)
;; (straight-use-package 'monokai-theme)
;; (load-theme 'monokai t)
(straight-use-package 'afternoon-theme)
(load-theme 'afternoon t)

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

;;; Spell checking
(dolist (hook '(text-mode-hook org-mode-hook))
  (add-hook hook (lambda ()
                   (flyspell-mode 1)
                   (auto-fill-mode 0)
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

;;; Scroll bar
(straight-use-package 'sml-modeline)
(if (require 'sml-modeline nil 'noerror)
    (progn
      (sml-modeline-mode 1)
      (scroll-bar-mode -1))
  (scroll-bar-mode 1)
  (set-scroll-bar-mode 'right))

;; default to unified diffs
(setq diff-switches "-u")

;; Copy paste between apps
(setq x-select-enable-clipboard t)

;; Disable toolbar mode
(tool-bar-mode 0)

;; Menu bar
(global-set-key [f11] 'menu-bar-mode)

;; Switch buffers
(global-set-key (kbd "C-'") 'other-window)

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
(set-face-foreground 'mode-line "white")
(set-face-background 'mode-line "dark green")

;;; Highlight the line your are on.
(global-hl-line-mode 1)
(set-face-background 'hl-line "gray15")

;;; Cursor
(blink-cursor-mode 1)
(set-cursor-color "yellow")
(setq-default cursor-type '(bar . 3) blink-cursor-blinks 200)

;;; Lisp setup
(straight-use-package 'paredit)
(dolist (hook '(emacs-lisp-mode-hook
		lisp-mode-hook
		slime-repl-mode-hook))
  (add-hook hook #'(lambda () (paredit-mode 1))))

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
(straight-use-package 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(straight-use-package 'isend-mode)
(setq isend-forward-line nil)

;;; From https://www.emacswiki.org/emacs/SmoothScrolling
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed t) ;; accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;;; Magit
(straight-use-package 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;;; Javascript
(with-eval-after-load 'js
  (define-key js-mode-map (kbd "M-.") nil))
;;; Default to rjsx mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
;; (add-to-list 'auto-mode-alist '("\\.ts\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

;;; Function to send current statement or function to nodejs repl.
;; (defun wb-nodejs-repl-send-statement ()
;;   (interactive)
;;   (save-excursion
;;     (mark-defun)
;;     (nodejs-repl-send-region (region-beginning) (region-end))
;;     (deactivate-mark)))

;; (add-hook 'rjsx-mode-hook
;;           (lambda ()
;;             (define-key js-mode-map (kbd "C-x C-e") 'nodejs-repl-send-last-expression)
;;             (define-key js-mode-map (kbd "C-c C-j") 'nodejs-repl-send-line)
;;             (define-key js-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
;;             (define-key js-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
;;             (define-key js-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl)
;;             (define-key js-mode-map (kbd "C-c C-b") 'nodejs-repl-send-buffer)
;;             (define-key js-mode-map (kbd "C-c C-c") 'wb-nodejs-repl-send-statement)
;;             (define-key js-mode-map (kbd "M-.") 'lsp-goto-type-definition)
;;             (electric-pair-local-mode t)))

;;; Company mode map
(add-hook 'company-mode-hook
          (lambda ()
            (define-key company-active-map (kbd "C-n") 'company-select-next)
            (define-key company-active-map (kbd "C-p") 'company-select-previous)))


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

(global-set-key [f12] 'william-bruschi/toggle-fullscreen)

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
(defun william-bruschi/local-sql-mode-hook-fun ()
  (setq tab-stop-list (number-sequence 2 120 2))
  (setq tab-width 2)
  (setq indent-tabs-mode nil))

(add-hook 'sql-mode-hook 'william-bruschi/local-sql-mode-hook-fun)

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
