;;; Org mode

;;; TODO: org-import should be able to parse "|" in CSV files.

;; REVIEW: We should not need this, but org-agenda complains without it.
(require 'org-duration)

(define-key org-mode-map (kbd "C-c C-a") 'org-agenda)

(setq
 ;; Disable line splitting on M-RET.
 org-M-RET-may-split-line '((default))
 org-insert-heading-respect-content t
 org-enforce-todo-dependencies t
 org-deadline-warning-days 7
 org-agenda-default-appointment-duration 60
 org-agenda-columns-add-appointments-to-effort-sum t
 org-ellipsis " […]"
 org-adapt-indentation nil
 ;; Add keywords.
 org-todo-keywords '((sequence "TODO" "REVIEW" "DONE"))
 ;; org-todo-keyword-faces '(("REVIEW" :inherit org-done))
 ;; Priorities.
 org-priority-start-cycle-with-default nil
 org-default-priority 67
 ;; Org-mode aligns text.
 indent-tabs-mode nil

 org-cycle-separator-lines 0
 org-catch-invisible-edits 'smart)

;;; Agendas.
(add-to-list 'org-agenda-files (expand-file-name "todo/todo.org.gpg"(getenv "PERSONAL")))
(load (expand-file-name "todo/agenda-list.el" (getenv "PERSONAL"))'noerror)

;;; Set PDF association in Org-mode (original is 'default).
(setcdr (assoc "\\.pdf\\'" org-file-apps) 'emacs)

;;; Hooks.
(dolist (fun '(;; turn-off-auto-fill
               ambrevar/turn-off-indent-tabs))
  (add-hook 'org-mode-hook fun))

(when (require 'org-contacts nil t)
  (let ((contacts (expand-file-name "contacts/contacts.org.gpg" (getenv "PERSONAL"))))
    (when (file-exists-p contacts)
      ;; When used to auto-complete e-mail addresses, the file is automatically
      ;; loaded.  The buffer usually need not be restored by a desktop session.
      (when (and desktop-save-mode
                 (string-match "\\\\|" desktop-files-not-to-save))
        (setq desktop-files-not-to-save
              (concat (substring desktop-files-not-to-save 0 -2) "\\|" (regexp-quote (expand-file-name contacts)) "\\)")))
      (setq org-contacts-files (list contacts)))))

(when (require 'org-bullets nil t)
  (add-hook 'org-mode-hook 'org-bullets-mode))

(add-to-list
 'org-capture-templates
 `("w" "Web link" entry (file+headline ,(car org-agenda-files) "Links")
   "* %?%a\n:SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))\n"))

(provide 'init-org)
