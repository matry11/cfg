;;; mu4e

;;; REVIEW: Handle attachments in attached e-mails.
;;; See https://github.com/djcb/mu/issues/454#issuecomment-320616279.
;;; TODO: <tab> should go to next link in text e-mails too.

;; We need 'main' to setup pinentry-emacs for GPG.
(require 'main)
(require 'init-message)

(when (require 'mu4e-maildirs-extension nil t)
  (mu4e-maildirs-extension))

(defun ambrevar/mu4e-headers ()
  "Like `mu4e' but show the header view.
Default to unread messages if the header buffer does not already exist."
  (interactive)
  (mu4e~start)
  (if (get-buffer "*mu4e-headers*" )
      (switch-to-buffer "*mu4e-headers*")
    (mu4e-headers-search "flag:unread AND NOT flag:trashed")))

(setq
 ;; Attachments
 mu4e-attachment-dir "~/Downloads"
 mu4e-save-multiple-attachments-without-asking t

 ;; IMAP sync.
 mu4e-maildir "~/.cache/mail"
 mu4e-get-mail-command "mbsync -a"
 mu4e-update-interval 90
 mu4e-headers-auto-update nil ; Don't refresh so that we don't lose the current filter upon, e.g. reading e-mails.
 mu4e-change-filenames-when-moving t ; Preferred for mbsync according to the man page.

 ;; Don't bother me with context on startup.
 mu4e-context-policy nil

 ;; For reporting bugs, "C-x m", etc.
 mail-user-agent 'mu4e-user-agent
 mu4e-compose-dont-reply-to-self t

 ;; Display
 mu4e-headers-date-format "%F %R"
 mu4e-headers-fields '((:human-date . 16)
                       (:flags . 6)
                       (:size . 6)
                       (:mailing-list . 10)
                       (:from . 22)
                       (:subject))
 mu4e-headers-time-format "%R"
 mu4e-view-show-addresses t
 mu4e-view-show-images t
 mu4e-view-image-max-width 800
 mu4e-hide-index-messages t

 ;; Gmail-style threading.
 mu4e-headers-include-related t

 ;; Gmail likes format=flowed(?)
 ;; mu4e-compose-format-flowed

 ;; 'sent is good for most providers.  Gmail requires 'delete.
 mu4e-sent-messages-behavior 'sent

 ;; Because default completion can be extended (e.g. Helm, Ivy).
 mu4e-completing-read-function 'completing-read)

;;; Press "aV" to view in browser.
(add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;;; Custom bookmarks
(add-to-list 'mu4e-bookmarks
             '("maildir:\".*inbox.*\" size:1M.." "Big inbox messages" ?b))

;;; Unicode chars for decoration might cause issues with some fonts or in terminals.
;;; https://github.com/djcb/mu/issues/733
;;; https://github.com/djcb/mu/issues/1062
;; (setq mu4e-use-fancy-chars t)

;;; REVIEW: Sorting in ascending order is impeded by
;;; `mu4e-search-results-limit': the 500 oldest e-mails will be displayed first.
;;; https://github.com/djcb/mu/issues/809
;; (setq mu4e-headers-sort-direction 'ascending)
;;; Since we sort in ascending direction, we default to the end of buffer.
;; (add-hook 'mu4e-headers-found-hook 'end-of-buffer)

(add-hook 'mu4e-conversation-hook 'ambrevar/mu4e-select-dictionary)

;;; Make some e-mails stand out a bit.
(set-face-foreground 'mu4e-unread-face "yellow")
(set-face-attribute 'mu4e-flagged-face nil :inherit 'font-lock-warning-face)

;;; Confirmation on every mark execution is too slow to my taste.
(defun ambrevar/mu4e-mark-execute-all-no-confirm ()
  (interactive)
  (mu4e-mark-execute-all t))
(define-key mu4e-headers-mode-map "x" 'ambrevar/mu4e-mark-execute-all-no-confirm)

(when (require 'helm-mu nil t)
  (dolist (map (list mu4e-headers-mode-map mu4e-main-mode-map mu4e-view-mode-map))
    (define-key map "s" 'helm-mu)))

;;; Org captures
(when (require 'org-mu4e nil t)
  (dolist (map (list mu4e-view-mode-map mu4e-headers-mode-map))
    ;; Org mode has "C-c C-t" for 'org-todo.
    (define-key map (kbd "C-c C-t") 'org-mu4e-store-and-capture))
  (setq org-mu4e-link-query-in-headers-mode nil)

  (require 'init-org)                   ; For org-agenda-files
  (defun ambrevar/org-mail-date (&optional msg)
    (with-current-buffer (mu4e-get-headers-buffer)
      (mu4e-message-field (or msg (mu4e-message-at-point)) :date)))
  (add-to-list 'org-capture-templates
               `("t" "Mark e-mail in agenda" entry (file+headline ,(car org-agenda-files) "E-mails")
                 "* %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"++7d\" nil (ambrevar/org-mail-date)))\n%a\n")))

(defun ambrevar/mu4e-kill-ring-save-message-id (&optional msg)
  "Save MSG's \"message-id\" field to the kill-ring.
If MSG is nil, use message at point."
  (interactive)
  (kill-new (mu4e-message-field (or msg (mu4e-message-at-point)) :message-id)))

(load (expand-file-name "mail/mu4e.el" (getenv "PERSONAL")) :noerror)

(provide 'init-mu4e)
