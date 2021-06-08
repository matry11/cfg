;; Notmuch

(require 'init-message)
(require 'patch-notmuch)
(require 'init-notmuch-sync)

;; To find files matching email:
;;   notmuch search --output=files FOO
;; The following is good enough for multiple-account support if they use the
;; same SMTP server.
(setq notmuch-fcc-dirs
      '(("mail@ambrevar.xyz" . "mail/Sent +sent -inbox -unread")
        ("pierre@atlas.engineer" . "atlas/Sent +sent -inbox -unread")))

(setq notmuch-saved-searches
      `((:name "inbox" :query "tag:inbox and date:1w.." :key ,(kbd "i"))
        (:name "unread" :query "tag:unread and -tag:nyxt" :key ,(kbd "u"))
        (:name "unread-all" :query "tag:unread" :key ,(kbd "U"))
        (:name "flagged" :query "tag:flagged" :key ,(kbd "f"))
        (:name "sent" :query "tag:sent and date:1w.." :key ,(kbd "t"))
        (:name "drafts" :query "tag:draft" :key ,(kbd "d"))
        (:name "all mail" :query "date:2w.." :key ,(kbd "a"))))

(defun ambrevar/notmuch-change-sender (&optional sender)
  (interactive)
  (unless (derived-mode-p 'message-mode)
    (error "Must be in message mode"))
  (unless sender
    (setq sender (completing-read "Sender: " (mapcar 'car notmuch-fcc-dirs))))
  (message-replace-header "From" sender)
  (message-remove-header "Fcc")
  (notmuch-fcc-header-setup))

(when (require 'helm-notmuch nil t)
  (setq helm-notmuch-match-incomplete-words t)
  (dolist (map (list notmuch-search-mode-map
                     notmuch-hello-mode-map
                     notmuch-show-mode-map
                     notmuch-tree-mode-map))
    (define-key map "s" 'helm-notmuch))
  (define-key notmuch-show-mode-map (kbd "M-s f") #'helm-imenu))

(when (require 'ol-notmuch nil 'noerror)
  (dolist (map (list notmuch-show-mode-map notmuch-tree-mode-map))
    (define-key map (kbd "C-c C-t") 'org-capture))
  (add-to-list 'org-capture-templates
               `("t" "Mark e-mail in agenda" entry (file+headline ,(car org-agenda-files) "E-mails")
                 "* %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"++7d\" nil (notmuch-show-get-date)))\n%a\n")))


(defun notmuch-show-bounce (&optional address)
  "Bounce the current message."
  (interactive "sBounce To: ")
  (notmuch-show-view-raw-message)
  (message-resend address))

(define-key notmuch-show-mode-map "b" #'notmuch-show-bounce)

;; Improve address completion with Helm.
(setq notmuch-address-use-company nil)
(setq notmuch-address-selection-function
      (lambda (prompt collection initial-input)
        (completing-read prompt (cons initial-input collection) nil t nil 'notmuch-address-history)))


;; The following can be used to use notmuch with debbugs, but it won't retrieve
;; the emails so this has to be done separately.
(defun debbugs-notmuch-select-report (&rest _)
  (let* ((status (debbugs-gnu-current-status))
         (id (cdr (assq 'id status)))
         (merged (cdr (assq 'mergedwith status))))
    (setq merged (if (listp merged) merged (list merged)))
    (unless id
      (user-error "No bug report on the current line"))
    (let ((address (format "%s@debbugs.gnu.org" id))
          (merged-addresses (string-join (mapcar (lambda (id)
                                                   (format "%s@debbugs.gnu.org %s" id))
                                                 merged)
                                         " ")))
      (notmuch-search (format "%s %s" address merged-addresses)))))

(defun ambrevar/notmuch-poll-async ()
  "Like `notmuch-poll' but asynchronous."
  (notmuch-start-notmuch
   "notmuch-new"
   nil
   (lambda (_proc change)
     (with-current-buffer (cl-find-if (lambda (b)
                                     (with-current-buffer b
                                       (eq major-mode 'notmuch-search-mode)))
                                   (buffer-list))
       (notmuch-refresh-this-buffer))
     (message "notmuch-new: %s" change))
   "new"))

;; TODO: This is a bit brittle since it only works if the given gpg file exists.
;; Is there a way to unlock gpg manually without a file?
(defun ambrevar/notmuch-poll-after-gpg-unlock ()
  "Unlock GPG and get Notmuch mail."
  ;; The gpg unlock needs to be asynchronous for EXWM, or else pinentry-emacs
  ;; will be blocked.
  (let ((sentinel (lambda (_process _args)
                    (ambrevar/notmuch-poll-async))))
    (make-process :name "gpg" :buffer nil
                  :command (list "gpg" "--decrypt"
                                 (expand-file-name
                                  (or (cl-find-if (lambda (agenda) (string-suffix-p ".gpg" agenda))
                                               org-agenda-files)
                                      (error "No .gpg file in `org-agenda-files'."))))
                  :sentinel sentinel)))

(advice-add 'notmuch-poll-and-refresh-this-buffer
            :override #'ambrevar/notmuch-poll-after-gpg-unlock)

;; (advice-add 'debbugs-gnu-select-report :override #'debbugs-notmuch-select-report)

;; Extend `notmuch-show-stash-mlarchive-link':
(defvar ambrevar/known-mailing-list-archives
  '(("help-guix@gnu.org" . "guix")
    ("guix-devel@gnu.org" . "guix")
    ("debbugs.gnu.org" . "guix"))
  "Alist of mail adresses and their Yhetil name.
Alternatively the key may just be a host name against which a
recipient will be matched.")

(defun ambrevar/guess-yhetil-link (message-id)
  (let* ((all-addresses (mapcar #'second
                                (mail-extract-address-components
                                 (mapconcat #'identity
                                            (list
                                             (notmuch-show-get-header :To)
                                             (notmuch-show-get-header :Cc))
                                            ", ")
                                 'all)))
         (match-address (lambda (address-or-host)
                          (if (string-match "@" address-or-host)
                              (member address-or-host all-addresses)
                            (seq-find (lambda (address)
                                        (string-match address-or-host address))
                                      all-addresses))))
         (mailing-list (alist-get
                        (seq-find match-address
                                  (mapcar #'car ambrevar/known-mailing-list-archives))
                        ambrevar/known-mailing-list-archives
                        nil nil #'string=)))
    (when mailing-list
      (concat "https://yhetil.org/"
              mailing-list "/" message-id))))

(add-to-list 'notmuch-show-stash-mlarchive-link-alist
             (cons "Yhetil" #'ambrevar/guess-yhetil-link))

(setq notmuch-show-stash-mlarchive-link-default "Yhetil")

(provide 'init-notmuch)
