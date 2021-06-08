;;; Main options

(require 'functions)

;;; Minimal UI. Run early to hide it as soon as possible.
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
;;; `tool-bar-mode' and `scroll-bar-mode' might not be compiled in.
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;;; In some cases, Emacs can still decide by itself to use graphical boxes.
;;; Force on using the minibuffer instead.
(setq use-dialog-box nil)

(setq
 recentf-max-saved-items 100
 disabled-command-function nil          ; Enable all disabled commands.
 scroll-error-top-bottom t              ; Alternative scrolling
 kill-whole-line t                      ; Kill whole line including \n.
 scroll-step 1                          ; Line by line scrolling
 calendar-week-start-day 1
 calendar-date-style 'iso
 comint-prompt-read-only t
 comint-input-ignoredups t
 woman-fill-column fill-column
 abbrev-file-name (expand-file-name "abbrev_defs" (getenv "PERSONAL"))
 frame-title-format (concat "%b" (unless (daemonp) " [serverless]"))
 delete-by-moving-to-trash t
 uniquify-buffer-name-style 'forward
 vc-follow-symlinks t       ; Disable prompt (but leave warning) on git symlink.
 sort-fold-case t

;;; Clipboard and primary selection.
 select-enable-primary t
 save-interprogram-paste-before-kill t

;;; Save all visited URLs.
 url-history-track t
 url-history-file (expand-file-name "url/history" user-emacs-directory)

 ;; History duplicates are useless (?) and annoying with recent Helm:
 ;; https://github.com/syl20bnr/spacemacs/issues/13564
 history-delete-duplicates t

 ;; Timeout before echoing the prefix of an unfinished keystroke.
 echo-keystrokes 0.5

 ;; Disable autosave features.
 auto-save-default nil
 auto-save-list-file-prefix nil

 ;; Place backup files in specific directory.
 backup-directory-alist
 `(("." . ,(expand-file-name "backups" user-emacs-directory)))

 ;; Enforce horizontal splitting. 140 means that the window is large enough to
 ;; hold 2 other windows of 70 columns.
 split-height-threshold nil
 split-width-threshold 140

 ;; TODO: Ediff does not seem to auto-refine.  Bug?  Compare daemon and no-daemon.
 ediff-window-setup-function 'ediff-setup-windows-plain
 ediff-split-window-function 'split-window-horizontally)

;; Bookmarks
(setq bookmark-save-flag 1)
(let ((personal-bookmarks (expand-file-name "bookmarks/emacs-bookmarks.el.gpg"
                                            (getenv "PERSONAL"))))
  (when (file-exists-p personal-bookmarks)
    (setq bookmark-default-file personal-bookmarks)))

;;; Remember last cursor position.
(save-place-mode)
;;; When the daemon is killed abruptly, places are not saved. Adding this hook
;;; allows to save places at a strategic moment.
(add-hook 'before-save-hook 'save-place-kill-emacs-hook)

(savehist-mode) ; Save M-: history.

(setq-default major-mode 'text-mode) ; Default mode.

;;; Disable suspend key since it is useless on Emacs server.
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(defalias 'yes-or-no-p 'y-or-n-p) ; Make questions less annoying.

(size-indication-mode 1) ; Print buffer size in mode line.

;;; Display defun in mode line.
;; (which-function-mode)

;;; No need when we have a status bar.
;; (display-time)
;; (setq display-time-day-and-date t
;;       display-time-24hr-format t
;;       display-time-default-load-average nil)

;;; Just like time, no need when we have a status bar.
;; (display-battery-mode)
;;; TODO: Battery status (%b) does not work properly.
;; (setq battery-mode-line-format "[%p%%%b %t]")

;;; Line numbers
;;; Adding to `find-file-hook' ensures it will work for every file, regardless of
;;; the mode, but it won't work for buffers without files nor on mode change.
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook 'ambrevar/turn-on-column-number-mode)
  (add-hook hook 'ambrevar/turn-off-line-number-mode)
  (add-hook hook 'display-line-numbers-mode))
(setq display-line-numbers-type 'visual)
(defun ambrevar/turn-on-absolute-line-number ()
  (setq display-line-numbers-type t))
(setq auto-revert-interval 1)
(add-hook 'auto-revert-tail-mode-hook 'ambrevar/turn-on-absolute-line-number)

;;; Indentation
(setq-default tab-width 2)
(with-no-warnings (defvaralias 'standard-indent 'tab-width))
(setq-default indent-tabs-mode t)

(setq
 whitespace-style
 '(face empty indentation space-after-tab space-before-tab tab-mark trailing))
;;; REVIEW: `whitespace-report' will mistakenly always report empty lines at
;;; beginning and end of buffer as long as there is at least one empty line.
;;; `whitespace-cleanup' works properly however.
;;; Reported at http://debbugs.gnu.org/cgi/bugreport.cgi?bug=23740.
;; (setq whitespace-action '(report-on-bogus))

;;; Cycle spacing instead of just-one-space.  This frees M-\.
(global-set-key [remap just-one-space] 'cycle-spacing)

;;; Hippie expand
;; (global-set-key (kbd "M-/") 'hippie-expand)

;;; Abbreviation is like snippets: annoying at times, especially in
;;; prog-mode.  They are useful in text mode to avoid the sprawling of
;;; abbreviations.
(add-hook 'text-mode-hook 'abbrev-mode)

;;; Auto-fill
(when (getenv "MANWIDTH")
  (setq-default fill-column (string-to-number (getenv "MANWIDTH"))))
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;; Windmove mode
;;; By default, it allows easy window switching with Shift+arrows. I like to
;;; stick to the home-row, but to avoid shadowing other binding I exceptionaly use
;;; 'super' (normally reserved to the WM).
(when (fboundp 'windmove-default-keybindings)
  (ambrevar/global-set-keys
   "s-h" 'windmove-left
   "s-j" 'windmove-down
   "s-k" 'windmove-up
   "s-l" 'windmove-right))
(ambrevar/global-set-keys
 "s-o" 'delete-other-windows
 ;; "s-w" 'other-window
 "s-c" 'delete-window)

;; REVIEW: If xdg-open is not found, set Emacs URL browser to the environment browser,
;; or w3m if BROWSER is not set.
;; See https://debbugs.gnu.org/cgi/bugreport.cgi?bug=18986.
;; In Emacs 27, the BROWSER variable is still not checked.
(require 'browse-url)
(setq browse-url-generic-program (or
                                  (executable-find (or (getenv "BROWSER") ""))
                                  (when (executable-find "xdg-mime")
                                    (let ((desktop-browser (ambrevar/call-process-to-string "xdg-mime" "query" "default" "text/html")))
                                      (substring desktop-browser 0 (string-match "\\.desktop" desktop-browser))))
                                  (executable-find browse-url-mozilla-program)
                                  (executable-find browse-url-firefox-program)
                                  (executable-find browse-url-chromium-program)
                                  (executable-find browse-url-kde-program)
                                  (executable-find browse-url-conkeror-program)
                                  (executable-find browse-url-chrome-program)))
(setq browse-url-browser-function '(;; TODO: Display hyperspec in other window.
                                    ("http://www.lispworks.com/reference/HyperSpec/.*" . eww-browse-url)
                                    ("file:///.*HyperSpec.*" . eww-browse-url)
                                    ("." . browse-url-default-browser)))

;; shr
(setq shr-width (string-to-number (or (getenv "MANWIDTH") "80"))
      ;; If you're using a dark theme, and the messages are hard to read, it
      ;; can help to change the luminosity, e.g.:
      shr-color-visible-luminance-min 80
      browse-url-secondary-browser-function browse-url-browser-function)

;;; Extend MIME-types support for videos.
(with-eval-after-load 'mailcap
  (dolist (ext '((".webm" . "video/webm")
                 (".mp4" . "video/mp4")
                 (".flv" . "video/mp4")
                 (".ogv" . "video/ogg")
                 (".wmv" . "video/x-ms-wmv")
                 (".mkv" . "video/x-matroska")))
    (add-to-list 'mailcap-mime-extensions ext)))

;;; Default ispell dictionary. If not set, Emacs uses the current locale.
(setq ispell-dictionary "english")
(ambrevar/define-keys text-mode-map
                      "C-<f6>" 'ispell-change-dictionary
                      "<f6>" 'ispell-buffer)

;;; Show matching parenthesis
(show-paren-mode 1)
;;; By default, there’s a small delay before showing a matching parenthesis. Set
;;; it to 0 to deactivate.
(setq show-paren-delay 0
      show-paren-when-point-inside-paren t)

;;; Electric Pairs to auto-complete () [] {} "" etc. It works on regions.
;; (electric-pair-mode)

;;; Compilation bindings and conveniences.
(setq compilation-ask-about-save nil
      compilation-scroll-output 'first-error)
(with-eval-after-load 'compile
  ;; Making `compilation-directory' local only works with `recompile'
  ;; and if `compile' is never used. In such a scenario,
  ;; `compile-command' is not saved by `recompile' itself which adds a
  ;; lot of bookkeeping.
  ;; (make-variable-buffer-local 'compilation-directory)
  ;; (make-variable-buffer-local 'compile-history)
  (make-variable-buffer-local 'compile-command))
;;; Some commands ignore that compilation-mode is a "dumb" terminal and still display colors.
;;; Thus we render those colors.
(require 'ansi-color)
(defun ambrevar/compilation-colorize-buffer ()
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))
(add-hook 'compilation-filter-hook 'ambrevar/compilation-colorize-buffer)
(global-set-key (kbd "<f7>") 'previous-error)
(global-set-key (kbd "<f8>") 'next-error)
(defun ambrevar/compile-last-command ()
  (interactive)
  (compile compile-command))
(ambrevar/define-keys prog-mode-map
                      "C-<f6>" 'compile
                      ;; Do not use `recompile' since we want to change the compilation folder for the current buffer.
                      "<f6>" 'ambrevar/compile-last-command)

;;; Desktop-mode
;;; REVIEW: `desktop-kill' should not query the user in `kill-emacs-hook'.
;;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=28943
(defun ambrevar/desktop-setup (&rest _ignored)
  (when (or (not (boundp 'desktop-save-mode))
            (null desktop-save-mode))
    ;; (when (< emacs-major-version 27)
    ;;   ;; TODO: By default, Emacs<27 prompts for unsafe variable when loading desktop
    ;;   ;; which stucks the daemon.  Disable this behaviour.
    ;;   (defun ambrevar/enable-safe-local-variables ()
    ;;     (setq enable-local-variables t))
    ;;   (add-hook 'after-init-hook 'ambrevar/enable-safe-local-variables))
    (require 'desktop)        ; This adds a hook to `after-init-hook'.
    ;; (when (< emacs-major-version 27)
    ;;   (defun ambrevar/enable-all-local-variables ()
    ;;     (setq enable-local-variables :all))
    ;;   (add-hook 'after-init-hook 'ambrevar/enable-all-local-variables))
    (setq history-length 250
          ;; Default timer (30) is way too high: for somebody too frenzy, the timer
          ;; might never be saved.  See
          ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=28943.
          desktop-auto-save-timeout 5
          ;; desktop-restore-eager 4 ; Can be annoying as you don't have your last-loaded buffers immediately.
          ;; desktop-load-locked-desktop 'ask
          desktop-restore-frames nil
          desktop-save t)
    (desktop-save-mode)
    (desktop-read)
    ;; Don't save any file, for faster startup and less problems.
    (setq desktop-files-not-to-save ".")
    ;; Discarding PDFs and images makes it lighter.
    (add-to-list 'desktop-modes-not-to-save 'pdf-view-mode)
    (add-to-list 'desktop-modes-not-to-save 'image-mode)
    ;; Discard Elfeed since it may ask for PGP passphrase.
    (add-to-list 'desktop-modes-not-to-save 'elfeed-search-mode)
    ;; TODO: `compile-history' should be buffer local but that does not work.
    ;; http://user42.tuxfamily.org/compile-history-local/index.html
    ;; http://stackoverflow.com/questions/22995203/one-compile-command-per-buffer-not-directory
    ;; (add-to-list 'desktop-locals-to-save 'compile-history)
    (add-to-list 'desktop-locals-to-save 'compile-command)
    (add-to-list 'desktop-locals-to-save 'ispell-local-dictionary)))

;; Auto-load/save sessions only when running the daemon.  `server-running-p' is
;; only useful once the daemon is started and cannot be used for initialization.
;; Use `daemonp' instead if emacs is started with `--daemon', or add to the
;; server hook otherwise.
(when (daemonp)
  (ambrevar/desktop-setup))
;; If started with `server-start' instead of `--daemon'.
;; server-mode does not seem to have any post-start hook.
(advice-add 'server-start :after #'ambrevar/desktop-setup)

;;; Skeleton settings
(when (require 'patch-skeletons nil 'noerror)
;;; Do not expand abbrevs in skeletons.
  (setq-default skeleton-further-elements '((abbrev-mode nil)))
  (ambrevar/turn-on-skeleton-markers)
  (ambrevar/global-set-keys
   "C->" 'skeleton-next-position
   "C-<" 'skeleton-previous-position))

;;; Move mouse away.
;; (mouse-avoidance-mode 'banish)
;;; That binding is not very useful and gets in the way of C-<mouse-1>.
;; (global-unset-key (kbd "C-<down-mouse-1>"))

;;; Scroll zooming.
(setq text-scale-mode-step 1.1)

;;; Replace not-so-useful comment-dwim binding.
(global-set-key (kbd "M-;") 'comment-line)

;;; Replace `kill-buffer' binding by `kill-this-buffer'.
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;;; Display Time World
(setq
 zoneinfo-style-world-list
 '(("UTC" "-")
   ("Europe/Paris" "France Germany Sweden")
   ("Asia/Calcutta" "India")
   ("Indian/Mauritius" "Mauritius")
   ("Africa/Tunis" "Tunisia")
   ("Asia/Ho_Chi_Minh" "Vietnam")
   ("Australia/Melbourne" "Melbourne")
   ("Africa/Nairobi" "Uganda")
   ("America/New_York" "New York")
   ("America/Los_Angeles" "Los Angeles")))

;;; Initial scratch buffer message.
(require 'functions) ; For `ambrevar/fortune-scratch-message'.
(let ((fortune (ambrevar/fortune-scratch-message)))
  (when fortune
    (setq initial-scratch-message fortune)))

;;; Support for Emacs pinentry.
;;; Required for eshell/sudo and everything relying on GPG queries.
(setq epa-pinentry-mode 'loopback) ; This will fail if gpg>=2.1 is not available.
(when (require 'pinentry nil t)
  (pinentry-start))

;;; Edebug
;; (setq
;;  ;; REVIEW: Does not seem necessary, since '=' already displays the coverage.
;;  edebug-test-coverage t
;;  edebug-trace t)

(when (require 'so-long nil 'noerror)
  (global-so-long-mode 1))

(defun ambrevar/change-log-set-indent-rules ()
  (setq tab-width 2 left-margin 2))
(add-hook 'change-log-mode-hook 'ambrevar/change-log-set-indent-rules)

(when (require 'ws-butler nil 'noerror)
  (ws-butler-global-mode)
  (setq ws-butler-keep-whitespace-before-point nil))

(require 'auth-source-pass)
(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))
(add-to-list 'auth-sources 'password-store 'append)

;; Visual
;;; Font size
(when (fboundp 'tool-bar-mode)
  ;; (set-face-attribute 'default nil :height 100)
  (when (find-font (font-spec :name "DejaVu Sans Mono-12"))
    ;; Emacs raises an error if font is not found.
    (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-12"))))
;;; Cursor type: default (box) is visible and practical.
;; (setq-default cursor-type 'hollow)
(setq-default x-stretch-cursor t)
;;; Blinking cursor is on only when Emacs is not daemonized.
(blink-cursor-mode 0)

;; Add EncFS support (for M-x shell):
(when (require 'comint nil :noerror)
  (setq comint-password-prompt-regexp
        (concat
         "\\("
         comint-password-prompt-regexp
         "\\|^EncFS\\)")))

(provide 'main)
