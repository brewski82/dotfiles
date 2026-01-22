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

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ;; ("M-'" . consult-register-store)
         ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; https://github.com/minad/vertico?tab=readme-ov-file#completion-at-point-and-completion-in-region
  (setq completion-in-region-function #'consult-completion-in-region)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )

;; Enable Vertico.
(use-package vertico
  ;; :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package embark
  :ensure t

  ;; :bind
  ;; (("C-." . embark-act)         ;; pick some comfortable binding
  ;;  ("C-;" . embark-dwim)        ;; good alternative: M-.
  ;;  ("C-h B" . embark-bindings))
  ;; alternative for `describe-bindings'

  ;; :init

  ;; ;; Optionally replace the key help with a completing-read interface
  ;; (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  ;; Add Embark to the mouse context menu. Also enable `context-menu-mode'.
  ;; (context-menu-mode 1)
  ;; (add-hook 'context-menu-functions #'embark-context-menu 100)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(require 'cl-lib)

;;; Org
(require 'william-bruschi-org-setup nil nil)

(use-package yasnippet
  :config (yas-global-mode 1))

(use-package json-mode
  :config (setq js-indent-level 2))

(use-package which-key
  :config (which-key-mode -1))

(use-package tree-sitter-langs
  :ensure t)

(use-package yaml-mode)

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
  (beacon-size 120)
  (beacon-dont-blink-major-modes '(t
                                   magit-status-mode
                                   magit-popup-mode
                                   inf-ruby-mode
                                   mu4e-headers-mode
                                   gnus-summary-mode
                                   gnus-group-mode
                                   term-mode
                                   eshell-mode
                                   shell-mode
                                   comint-mode
                                   vterm-mode
                                   eat-mode)))

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
  :config (setq vterm-timer-delay nil)
  ;; https://github.com/akermu/emacs-libvterm/issues/765
  :config (define-key vterm-mode-map [return] nil t)
  :custom
  (vterm-max-scrollback 10000)
  (vterm-always-compile-module t)
  :hook (vterm-mode . (lambda ()
                        (setq buffer-face-mode-face '(:family "JuliaMono"))
                        (buffer-face-mode))))

;;; Scroll bar
(use-package sml-modeline
  :config
  (sml-modeline-mode 1)
  (scroll-bar-mode 1)
  (set-scroll-bar-mode 'right))

;; default to unified diffs
(setq diff-switches "-u")

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Refresh directories automatically upon visiting
(setq dired-auto-revert-buffer t)

;;; Preserve case when dynamically expanding
(setq dabbrev-case-replace nil)

;;; Bell
(setq visible-bell 1)

;;; Cursor
(blink-cursor-mode 1)
(set-cursor-color "yellow")
(setq-default cursor-type t blink-cursor-blinks 200)

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
  :custom
  (isend-forward-line t)
  (isend-end-with-empty-line t))

(use-package markdown-mode)

;;; From https://www.emacswiki.org/emacs/SmoothScrolling
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed t) ;; accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(setq sentence-end-double-space nil)

;;; Mac setup
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'meta)

;;; Remap Cmd V for paste on mac
(when (memq window-system '(mac ns))
  (global-set-key (kbd "M-v") 'yank)
  (global-set-key (kbd "C-S-v") 'scroll-down-command))

;; Copy paste between apps
(setq x-select-enable-clipboard t)

;;; Recent files
(recentf-mode 1)

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
 '(auto-dim-other-buffers-mode t)
 '(compilation-scroll-output t)
 '(completion-ignore-case t t)
 '(eglot-autoshutdown t)
 '(eglot-confirm-server-initiated-edits nil)
 '(eglot-events-buffer-size 0)
 '(eglot-sync-connect 0)
 '(enable-recursive-minibuffers t)
 '(fill-column 80)
 '(fringe-mode 10 nil (fringe))
 '(markdown-command william-bruschi/markdown-command)
 '(minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
 '(org-hide-emphasis-markers t)
 '(read-buffer-completion-ignore-case t)
 '(read-extended-command-predicate #'command-completion-default-include-p)
 '(recentf-max-saved-items 500)
 '(safe-local-variable-values
   '((william-bruschi/prettier . "npm run prettier")
     (william-bruschi/prettier . "npx prettier --write")
     (william-bruschi/prettier . "npx prettier")
     (william-bruschi/eslint . "npx eslint"))))

;;; Global Key bindings
(global-set-key (kbd "C-'") 'other-window)
(global-set-key (kbd "C-;") 'completion-at-point)
(global-set-key [f7] 'william-bruschi/magit-three-windows)
(global-set-key [f8] 'toggle-frame-maximized)
(global-set-key [f11] 'menu-bar-mode)
(global-set-key [f12] 'recompile)


(use-package all-the-icons
  :if (display-graphic-p))

(use-package spacemacs-theme
  :ensure t
  :config
  (load-theme 'spacemacs-dark t)
  :custom
  (spacemacs-theme-org-agenda-height t)
  (spacemacs-theme-org-highlight t)
  (spacemacs-theme-underline-parens nil))

;; Disable toolbar mode
(tool-bar-mode 0)

(electric-pair-mode 1)

;;; Highlight the line your are on.
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)

;;; https://github.com/mina86/auto-dim-other-buffers.el
(use-package auto-dim-other-buffers
  :config (auto-dim-other-buffers-mode 1)
  :custom-face (auto-dim-other-buffers ((t (:background "gray27")))))

;;; Support color in compilation mode. See
;;; https://stackoverflow.com/a/71785402q
(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package math-preview)

(menu-bar-mode 1)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t (:background "DarkOliveGreen4"))))
 '(mode-line ((t (:background nil))))
 '(mode-line-inactive ((t (:background "dim gray" :foreground "white")))))

(require 'william-bruschi-lisp)

(require 'william-bruschi-javascript nil nil)

;;; eglot config
(dolist (hook '(python-mode-hook rjsx-mode-hook typescript-mode-hook))
  (add-hook hook (lambda ()
                   (eglot-ensure)
                   (define-key eglot-mode-map (kbd "M-.") 'xref-find-definitions)
                   (define-key eglot-mode-map (kbd "C-.") 'xref-find-definitions)
                   (define-key eglot-mode-map (kbd "C-,") 'xref-go-back))))

(require 'william-bruschi-custom)
(require 'william-bruschi-ai-tools)
(require 'william-bruschi-init-post nil t)
