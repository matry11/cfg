;;; Functions

;;; Notes on mark and region: to get a consistent behaviour regardless of
;;; Transient mode, check `(use-region-p)'. It will work as expected if
;;; transient. If not, it will always be true as soon as the mark has been set
;;; once; so you need to make sure the mark is set as you want beforehand (e.g.
;;; whole buffer, single line...). This is the behaviour of `sort-lines'.
;;;
;;; The clean way to get static region boundaries and fallback on buffer boundaries:
;;
;; (let (start end)
;;   (if (use-region-p)
;;       (setq start (region-beginning) end (region-end))
;;     (setq start (point-min) end (point-max)))
;;
;;; If several commands act on region and the region size/pos is susceptible to change:
;;
;; (let ((start (set-marker (make-marker) (if (use-region-p) (region-beginning) (point-min))))
;;       (end (set-marker (make-marker) (if (use-region-p) (region-end) (point-end)))))
;;
;;; For commands that only work on regions:
;;
;; (defun count-lines-region (start end)
;;   "Print number of lines and characters in the region."
;;   (interactive "r")
;;   ...

(defun ambrevar/call-process-to-string (program &rest args)
  "Call PROGRAM with ARGS and return output.
See also `process-lines'."
  ;; Or equivalently:
  ;; (with-temp-buffer
  ;;   (apply 'process-file program nil t nil args)
  ;;   (buffer-string))
  (with-output-to-string
    (with-current-buffer standard-output
      (apply 'process-file program nil t nil args))))

(defun ambrevar/define-keys (map key def &rest bindings)
  "Like `define-key' but allow for defining several bindings at once.
`KEY' must be acceptable for `kbd'."
  (while key
    (define-key map (kbd key) def)
    (setq key (pop bindings)
          def (pop bindings))))

;; TODO: Bind this to ediff control panel.
(defun ambrevar/ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))

(defun ambrevar/escape-cl-wildcards (s)
  (replace-regexp-in-string "\\([]*?[]\\)" "\\\\\\1" s))

(defun ambrevar/escape-region (&optional regex to-string)
  "Escape double-quotes and backslashes.
This is useful for writing Elisp strings containing those
characters. The optional parameters let you control the replacement of REGEX for
TO-STRING."
  (interactive)
  (unless regex (setq regex "\\([\"\\\\]\\)"))
  (unless to-string (setq to-string "\\\\\\1"))
  (while (re-search-forward regex (if (use-region-p) (region-end) (point-max)) t)
    (replace-match to-string)))

(defun ambrevar/prettify ()
  "(Un)tabify, indent and delete trailing whitespace.

Tabify if `indent-tabs-mode' is true, otherwise use spaces.
Work on buffer or region.

Require `ambrevar/tabify-leading'."
  (interactive)
  (let ((start (set-marker (make-marker) (if (use-region-p) (region-beginning) (point-min))))
        (end (set-marker (make-marker) (if (use-region-p) (region-end) (point-max)))))
    (if indent-tabs-mode
        (ambrevar/tabify-leading)
      (untabify start end))
    (indent-region start end)
    (save-restriction
      (narrow-to-region start end)
      (delete-trailing-whitespace))))

(defun ambrevar/flyspell-and-whitespace-mode ()
  "Toggle `flyspell-mode' and `whitespace-mode'."
  (interactive)
  (if (derived-mode-p 'prog-mode)
      (flyspell-prog-mode)
    (flyspell-mode)
    (when flyspell-mode
      (flyspell-buffer)))
  (whitespace-mode 'toggle))
(global-set-key (kbd "<f9>") #'ambrevar/flyspell-and-whitespace-mode)

;;; From https://www.reddit.com/r/emacs/comments/70bn7v/what_do_you_have_emacs_show_when_it_starts_up/.
;;; Supply a random fortune cookie as the *scratch* message.
(defun ambrevar/fortune-scratch-message ()
  (interactive)
  (let ((fortune
         (when (executable-find "fortune")
           (with-temp-buffer
             (shell-command "fortune" t)
             (while (not (eobp))
               (insert ";; ")
               (forward-line))
             (delete-trailing-whitespace (point-min) (point-max))
             (concat (buffer-string) "\n")))))
    (if (called-interactively-p 'any)
        (insert fortune)
      fortune)))

(defun ambrevar/global-set-keys (key def &rest bindings)
  "Like `global-set-key' but allow for defining several bindings at once.
`KEY' must be acceptable for `kbd'."
  (while key
    (global-set-key (kbd key) def)
    (setq key (pop bindings)
          def (pop bindings))))

(defun ambrevar/kernel-format ()
  "Search all CONFIG_* string in the current buffer and replace
everything in a way that's suitable for kconfig."
  (interactive)
  (let ((list nil))
    (while (re-search-forward "\\<CONFIG_[_[:alnum:]]+" nil :noerror)
      (push (match-string 0) list))
    (erase-buffer)
    (dolist (entry list)
      (insert entry "=m")
      (newline))))

(defun ambrevar/image-display-external ()
  "Display original image at point using external viewer."
  (interactive)
  (let ((file (or (get-text-property (point) 'original-file-name)
                  (let ((image (get-text-property (point) 'display)))
                    (when image
                      (plist-get (cdr image) :file))))))
    (if (not file)
        (message "No original file name found")
      (start-process "image-dired-thumb-external" nil
                     image-dired-external-viewer (expand-file-name file)))))
(define-key image-map (kbd "S-<return>") 'ambrevar/image-display-external)

(defun ambrevar/current-minor-modes ()
  "Return the list of minor modes enabled in the current buffer."
  (interactive)
  (delq nil
        (mapcar (lambda (mode)
                  (if (and (boundp mode) (symbol-value mode))
                      mode))
                minor-mode-list)))

(defun ambrevar/local-set-keys (key def &rest bindings)
  "Like `local-set-key' but allow for defining several bindings at once.
`KEY' must be acceptable for `kbd'."
  (while key
    (local-set-key (kbd key) def)
    (setq key (pop bindings)
          def (pop bindings))))

(defun ambrevar/reset-fill-column ()
  "Reset `fill-column' to its default value."
  (interactive)
  (setq fill-column (default-value 'fill-column)))

(defun ambrevar/ring-delete-first-item-duplicates (ring)
  "Remove duplicates of last command in history.
Return RING.

This should be faster then `seq-uniq'.  Unlike
`eshell-hist-ignoredups' or `comint-input-ignoredups', it does
not allow duplicates ever.
Surrounding spaces are ignored when comparing."
  (let ((first (ring-ref ring 0))
        (index 1))
    (while (<= index (1- (ring-length ring)))
      (if (string= (string-trim first)
                   (string-trim (ring-ref ring index)))
          ;; REVIEW: We could stop at the first match, it would be faster and it
          ;; would eliminate duplicates if we started from a fresh history.
          ;; From an existing history that would not clean up existing
          ;; duplicates beyond the first one.
          (ring-remove ring index)
        (setq index (1+ index))))
    ring))

(defun ambrevar/sort-lines-unique (arg)
  "Remove trailing white space, then duplicate lines, then sort the result.
Do not fold case with \\[universal-argument] or non-nil ARG."
  (interactive "P")
  (let ((start (set-marker (make-marker) (if (use-region-p) (region-beginning) (point-min))))
        (end (set-marker (make-marker) (if (use-region-p) (region-end) (point-end)))))
    (let ((sort-fold-case (if arg nil t)))
      (delete-trailing-whitespace start end)
      (delete-duplicate-lines start end)
      (sort-lines nil start end))))

(defun ambrevar/tabify-leading ()
  "Call `tabify' on leading spaces only.
Works on whole buffer if region is unactive."
  (interactive)
  (require 'tabify) ; Need this to initialize `tabify-regexp'.
  (let ((tabify-regexp-old tabify-regexp) start end)
    (if (use-region-p)
        (setq start (region-beginning) end (region-end))
      (setq start (point-min) end (point-max)))
    (unwind-protect
        (progn
          (setq tabify-regexp "^\t* [ \t]+")
          (tabify start end))
      (setq tabify-regexp tabify-regexp-old))))

;; Inspired by https://github.com/abo-abo/oremacs.git.
(defun ambrevar/test-emacs ()
  "Return nil on error, t on success so that it can be added to
`kill-emacs-query-functions'."
  (interactive)
  (if (not user-init-file)
      (progn
        (message "No init file")
        t)
    (let ((output
           (shell-command-to-string
            (format "emacs --batch --eval \"
 (condition-case e
    (progn
      (load \\\"%s\\\")
      (message \\\"-OK-\\\"))
  (error
   (message \\\"ERROR:\\\")
   (signal (car e) (cdr e))))\""
                    user-init-file))))
      (if (string-match "-OK-" output)
          (progn
            (when (called-interactively-p 'any)
              (message "All is well"))
            t)
        (switch-to-buffer-other-window "*init file error*")
        (erase-buffer)
        (insert output)
        (search-backward "ERROR:")
        nil))))
(add-hook 'kill-emacs-query-functions 'ambrevar/test-emacs)

(defun ambrevar/toggle-org-reader ()
  "Toggle `variable-pitch-mode' and other niceties for Org reading."
  (interactive)
  (make-variable-buffer-local 'org-hide-emphasis-markers)
  ;; Because we revert the buffer, we need to set org-hide-emphasis-markers twice.
  (if org-hide-emphasis-markers
      (progn
        (follow-mode -1)
        (setq org-hide-emphasis-markers nil)
        (setq display-line-numbers-type 'visual)
        (revert-buffer)
        (setq org-hide-emphasis-markers nil)
        (variable-pitch-mode -1))
    (split-window-right)
    (follow-mode)
    (setq org-hide-emphasis-markers t)
    (setq display-line-numbers-type t)
    (revert-buffer)
    (setq org-hide-emphasis-markers t)
    (variable-pitch-mode 1)))

(defun ambrevar/toggle-word-delim ()
  "Make underscore part of the word syntax or not.
This does not interfere with `subword-mode'."
  (interactive)
  (if (equal (char-syntax ?_) "_")
      (progn
        (modify-syntax-entry ?_ "w")
        (message "_ is a not word delimiter"))
    (modify-syntax-entry ?_ "_")
    (message "_ is a word delimiter")))

(defun ambrevar/youtube-dl-url (&optional url)
  "Run 'youtube-dl' over the URL.
If URL is nil, use URL at point."
  (interactive)
  (setq url (or url (thing-at-point-url-at-point)))
  (let ((eshell-buffer-name "*youtube-dl*")
        (directory (seq-find (lambda (dir)
                               (and (file-directory-p dir) (expand-file-name dir)))
                             '("~/Videos" "~/Downloads")
                             ".")))
    (eshell)
    (when (eshell-interactive-process)
      (eshell t))
    (eshell-interrupt-process)
    (insert (format " cd '%s' && youtube-dl " directory) url)
    (eshell-send-input)))

(defvar ambrevar/privoxy-services '(("http" . "127.0.0.1:8118")
                                    ("https" . "127.0.0.1:8118"))
  "See `url-proxy-services' and `ambrevar-toggle-proxy'.")

(defun ambrevar/toggle-proxy ()
  "Toggle `url-proxy-services' between `ambrevar/privoxy-services' and nil."
  (interactive)
  (require 'url)
  (setq url-proxy-services
        (if (or url-proxy-services
                (not (member "privoxy"
                             (mapcar (lambda (p) (alist-get 'comm (process-attributes p)))
                                     (list-system-processes)))))
            (progn
              (message "Proxy disabled.")
              nil)
          (progn
            (message "Proxy enabled.")
            ambrevar/privoxy-services))))

(provide 'functions)
