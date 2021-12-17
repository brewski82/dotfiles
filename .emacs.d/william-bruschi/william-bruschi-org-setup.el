;;; Org Mode Setup

(straight-use-package 'org)
(straight-use-package 'org-bullets)
(require 'org-bullets)
(add-hook 'org-mode-hook 'org-bullets-mode)

;; (require 'ox-extra)
;; (ox-extras-activate '(ignore-headlines))
;; (require 'ox-latex)
;; (setq org-latex-inputenc-alist '(("utf8" . "utf8x")))
;; (setf (nth 4 org-emphasis-regexp-components) 10)

(setq org-default-notes-file "~/Documents/org/notes.org")
(global-set-key (kbd "C-c c") #'org-capture)

(dolist (hook '(org-mode-hook))
  (add-hook hook (lambda ()
		   (local-set-key (kbd "C-c t") 'org-time-stamp-inactive))))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "" "Tasks")
         "* TODO %?\n  %u\n%i\n")
        ("n" "Notes" entry (file+headline "" "Notes")
         "* %?\n  %u\n%i\n")))

(require 'william-bruschi-org-agenda nil nil)

(setq org-refile-targets
      '((nil . (:level . 1))
        (org-agenda-files :level . 1)))

(provide 'william-bruschi-org-setup)
