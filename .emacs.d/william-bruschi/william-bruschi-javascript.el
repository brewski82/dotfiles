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

(defun william-bruschi/scspell-current-buffer ()
  "Spell check the current buffer using scspell, the source code
spell checker."
  (interactive)
  (let ((command (concat "scspell --report-only " (buffer-file-name))))
    (william-bruschi/run-compile command)))

(define-transient-command william-bruschi/typescript-jest ()
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

(define-transient-command william-bruschi/typescript-prettier ()
  "Prettier Commands"
  [ "Prettier Current"
     ("f" "file" prettier-prettify)
     ("r" "region" prettier-prettify-region)])

(define-transient-command william-bruschi/typescript-lsp ()
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

(defun william-bruschi/run-eslint-command (command)
  "Runs an eslint command"
  (jest-test-from-project-directory (buffer-file-name)
    (let ((comint-scroll-to-bottom-on-input t)
          (comint-scroll-to-bottom-on-output t)
          (comint-process-echoes t))
      ;; TODO: figure out how to prevent <RET> from re-sending the old input
      ;; See https://stackoverflow.com/questions/51275228/avoid-accidental-execution-in-comint-mode
      (compilation-start command t))))

(defun william-bruschi/eslint-current-package ()
  "Runs eslint against the current package"
  (interactive)
  (william-bruschi/run-eslint-command "eslint ."))

(defun william-bruschi/eslint-current-package-myconfig ()
  "Runs eslint against the current package"
  (interactive)
  (william-bruschi/run-eslint-command "eslint -c ~/.my-eslint-config.yml ."))

(define-transient-command william-bruschi/typescript-trainsient ()
  "Typescript Commands"
  [[ "Actions"
     ("c" "package command" run-command)
     ("a" "action" lsp-execute-code-action)
     ("r" "rename" lsp-rename)]
   [ "Test"
     ("p" "package" jest-test-run-all-tests)
     ("t" "file" jest-test-run)]
   [ "Docs"
     ("d" "describe" lsp-describe-thing-at-point)
     ("g" "glance" lsp-ui-doc-glance)]
   [ "Format"
     ("f" "file" (lambda ()
                   (interactive)
                   (lsp-format-buffer)
                   (prettier-prettify)))
     ("R" "region" (lambda ()
                     (interactive)
                     (lsp-format-region)
                     (prettier-prettify-region)))]
   [ "Lint"
     ("e" "project config" william-bruschi/eslint-current-package)
     ("m" "my config" william-bruschi/eslint-current-package-myconfig)]
   [ "More"
     ("l" "lsp" william-bruschi/typescript-lsp)
     ("j" "jest" william-bruschi/typescript-jest)]
   [ "Other"
     ("s" "spell" william-bruschi/scspell-current-buffer)]])

(dolist (hook '(typescript-mode-hook rjsx-mode-hook))
  (add-hook hook
            (lambda ()
              (local-set-key (kbd "C-c C-c") 'william-bruschi/typescript-trainsient))))

;;; Sending commands to vterm
;; (vterm "my-window")
;; (isend--send-dest "echo test" (get-buffer "my-window"))

(provide 'william-bruschi-javascript)
