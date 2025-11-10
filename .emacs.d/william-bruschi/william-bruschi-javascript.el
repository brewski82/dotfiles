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
(use-package jest-test-mode
  :hook ((rjsx-mode typescript-mode) . jest-test-mode)
  :custom
  (jest-test-command-string "yarn run %s jest %s %s"))
(use-package prettier)


;;; See https://gist.github.com/abrochard/dd610fc4673593b7cbce7a0176d897de for examples.
(require 'magit)

(defun william-bruschi/run-compile (command)
  "Run compilation COMMAND with my settings."
  (let ((comint-scroll-to-bottom-on-input t)
        (comint-scroll-to-bottom-on-output t)
        (comint-process-echoes t))
    ;; TODO: figure out how to prevent <RET> from re-sending the old input
    ;; See https://stackoverflow.com/questions/51275228/avoid-accidental-execution-in-comint-mode
    (compilation-start command t)))

(defun william-bruschi/run-compile-from-package-root (command)
  "Runs command from npm package root dir"
  (jest-test-from-project-directory (buffer-file-name)
    (william-bruschi/run-compile command)))

(defun william-bruschi/scspell-current-buffer ()
  "Spell check the current buffer using scspell, the source code
spell checker."
  (interactive)
  (let ((command (concat "scspell --report-only " (buffer-file-name))))
    (william-bruschi/run-compile command)))

(transient-define-prefix william-bruschi/typescript-jest ()
  "Jest Test Commands"
  ["Test"
   [ "Current"
     ("f" "file" jest-test-run)
     ("d" "describe block" jest-test-run-at-point)]
   [ "Package"
     ("a" "all tests" jest-test-run-all-tests)
     ("r" "re-run last" jest-test-rerun-test)]
   [ "Debug"
     ("q" "debug file" jest-test-debug)
     ("w" "debug current" jest-test-debug-run-at-point)
     ("e" "debug last" jest-test-debug-rerun-test)]])

(transient-define-prefix william-bruschi/typescript-lsp ()
  "LSP Commands"
  [["Workspace"
    ("w" "add" lsp-workspace-folders-add)
    ("W" "remove" lsp-workspace-folders-remove)
    ("I" "describe" lsp-describe-session)]
   ["Treemacs"
    ("e" "errors" lsp-treemacs-errors-list)
    ("c" "call hierarchy" lsp-treemacs-call-hierarchy)]
   ["Session"
    ("K" "disconnect" lsp-disconnect)
    ("Q" "shutdown" lsp-workspace-shutdown)
    ("Z" "restart" lsp-workspace-restart)]]
  ["Find"
   [("A" "apropro" xref-find-apropos)
    ("d" "definition" lsp-find-definition)
    ("i" "impl" lsp-find-implementation)]
   [("x" "references" lsp-find-references)
    ("t" "type def" lsp-find-type-definition)]]
  ["Action"
   [("a" "execute" lsp-execute-code-action)
    ("h" "highlight" lsp-document-highlight)
    ("r" "rename" lsp-rename)]
   [("o" "organize imports" lsp-organize-imports)]]
  [["Doc"
    ("G" "glance" lsp-ui-doc-glance)
    ("H" "describe" lsp-describe-thing-at-point)]
   ["Peek"
    ("D" "definition" lsp-ui-peek-find-definitions)
    ("X" "references" lsp-ui-peek-find-references)
    ("S" "symbols" lsp-ui-peek-find-workspace-symbol)]])

(transient-define-prefix william-bruschi/typescript-eglot ()
  "Eglot Commands"
  [["Workspace"
    ("K" "shutdown" eglot-shutdown)
    ("R" "reconnect" eglot-reconnect)]
   ["Flymake"
    ("e" "errors" flymake-show-buffer-diagnostics)
    ("E" "project errors" flymake-show-project-diagnostics)]]
  ["Find"
   [("A" "apropro" xref-find-apropos)]
   [("x" "references" xref-find-references)
    ("t" "type def" xref-find-definitions)]]
  ["Action"
   [("a" "actions" eglot-code-actions)
    ("o" "organize imports" eglot-code-action-organize-imports)
    ("r" "rename" eglot-rename)]
   [("f" "format" eglot-format)]]
  [["Doc"
    ("d" "describe" eldoc)]
   ])

(defcustom william-bruschi/prettier "prettier --write"
  "command to run prettier"
  :safe (lambda (x) t))

(defun william-bruschi/run-prettier-command (command)
  "Runs a prettier command"
  (william-bruschi/run-compile-from-package-root
   (concat william-bruschi/prettier " " command)))

(defun william-bruschi/run-prettier-current-file ()
  "Runs a prettier on current file"
  (william-bruschi/run-prettier-command (buffer-file-name)))

(defcustom william-bruschi/eslint "eslint"
  "command to run eslint"
  :safe (lambda (x) t))

(defun william-bruschi/run-eslint-command (command)
  "Runs an eslint command"
  (william-bruschi/run-compile-from-package-root
   (concat william-bruschi/eslint " " command)))

(defun william-bruschi/eslint-current-package ()
  "Runs eslint against the current package"
  (interactive)
  (william-bruschi/run-eslint-command "."))

(defun william-bruschi/eslint-current-package-myconfig ()
  "Runs eslint against the current package"
  (interactive)
  (william-bruschi/run-eslint-command "-c ~/.my-eslint-config.yml ."))

(defun william-bruschi/eslint-current-file ()
  "Runs eslint against the current file"
  (interactive)
  (william-bruschi/run-eslint-command (buffer-file-name)))

(defun william-bruschi/eslint-current-file-myconfig ()
  "Runs eslint against the current package"
  (interactive)
  (william-bruschi/run-eslint-command
   (concat "-c ~/.my-eslint-config.yml " (buffer-file-name))))

(transient-define-prefix william-bruschi/typescript-trainsient ()
  "Typescript Commands"
  [[ "Actions"
     ("c" "package command" run-command)
     ("a" "action" eglot-code-actions)
     ("r" "rename" eglot-rename)]
   [ "Test"
     ("p" "package" jest-test-run-all-tests)
     ("t" "file" jest-test-run)]
   [ "Docs"
     ("d" "describe" eldoc)]
   [ "Format"
     ("f" "file" (lambda ()
                   (interactive)
                   (save-buffer)
                   (william-bruschi/run-prettier-current-file)
                   (revert-buffer t t)))]
   [ "Lint"
     ("e" "project" william-bruschi/eslint-current-package)
     ("E" "Me project" william-bruschi/eslint-current-package-myconfig)
     ("w" "file" william-bruschi/eslint-current-file)
     ("W" "Me file" william-bruschi/eslint-current-file-myconfig)]
   [ "More"
     ("g" "eglot" william-bruschi/typescript-eglot)
     ("j" "jest" william-bruschi/typescript-jest)]
   [ "Other"
     ("s" "spell" william-bruschi/scspell-current-buffer)
     ("i" "imenu" helm-imenu)]])

(defun william-bruschi/run-typescript-menu ()
  (interactive)
  (save-buffer)
  (william-bruschi/typescript-trainsient))

(dolist (hook '(typescript-mode-hook rjsx-mode-hook))
  (add-hook hook
            (lambda ()
              (local-set-key (kbd "C-c C-c") 'william-bruschi/run-typescript-menu))))

;;; Sending commands to vterm
;; (vterm "my-window")
;; (isend--send-dest "echo test" (get-buffer "my-window"))

;;; Javascript
(with-eval-after-load 'js
  (define-key js-mode-map (kbd "M-.") nil))
;;; Default to rjsx mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
;; (add-to-list 'auto-mode-alist '("\\.ts\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

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

(provide 'william-bruschi-javascript)
