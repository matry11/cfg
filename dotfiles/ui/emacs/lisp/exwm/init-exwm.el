;;; EXWM

;;; When stating the client from .xinitrc, `save-buffer-kill-terminal' will
;;; force-kill Emacs before it can run through `kill-emacs-hook'.
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)

;;; Athena+Xaw3d confuses xcape when binding Caps-lock to both L_Ctrl
;;; escape, in which case it will procude <C-escape> in Emacs. In practice, it
;;; means that `C-` keys will works but `<escape>` will need a fast double tap
;;; on Caps Lock.
;;;
;;; See https://github.com/ch11ng/exwm/issues/285
;;; and https://gitlab.com/interception/linux/plugins/caps2esc/issues/2.

;; Looks like there is a bug between Helm and EXWM global keys which leads Helm
;; to display nothing when only `exwm-input-set-key' is used.
;; https://github.com/ch11ng/exwm/issues/816
(defun ambrevar/exwm-global-set-key (keys command)
  "Bind KEYS to COMMAND.
KEYS is passed to `kbd'."
  (define-key exwm-mode-map (kbd keys) command)
  (global-set-key (kbd keys) command))

;;; Rename buffer to window title.
(defun ambrevar/exwm-rename-buffer-to-title () (exwm-workspace-rename-buffer exwm-title))
(add-hook 'exwm-update-title-hook 'ambrevar/exwm-rename-buffer-to-title)

(add-hook 'exwm-floating-setup-hook 'exwm-layout-hide-mode-line)
(add-hook 'exwm-floating-exit-hook 'exwm-layout-show-mode-line)

;;; Allow non-floating resizing with mouse.
(setq window-divider-default-bottom-width 2
      window-divider-default-right-width 2)
(window-divider-mode)

;;; System tray
(require 'exwm-systemtray)
(exwm-systemtray-enable)
(setq exwm-systemtray-height 16)

;;; Those cannot be set globally: if Emacs would be run in another WM, the "s-"
;;; prefix will conflict with the WM bindings.
(exwm-input-set-key (kbd "s-R") #'exwm-reset)
(exwm-input-set-key (kbd "s-x") #'exwm-input-toggle-keyboard)
(exwm-input-set-key (kbd "s-h") #'windmove-left)
(exwm-input-set-key (kbd "s-j") #'windmove-down)
(exwm-input-set-key (kbd "s-k") #'windmove-up)
(exwm-input-set-key (kbd "s-l") #'windmove-right)
(exwm-input-set-key (kbd "s-D") #'kill-this-buffer)
(ambrevar/exwm-global-set-key "s-b" #'list-buffers)
(ambrevar/exwm-global-set-key "s-f" #'find-file)

(when (require 'windower nil 'noerror)
  (exwm-input-set-key (kbd "s-<tab>") 'windower-switch-to-last-buffer)
  (exwm-input-set-key (kbd "s-o") 'windower-toggle-single)
  (exwm-input-set-key (kbd "s-\\") 'windower-toggle-split)
  (exwm-input-set-key (kbd "s-H") 'windower-swap-left)
  (exwm-input-set-key (kbd "s-J") 'windower-swap-below)
  (exwm-input-set-key (kbd "s-K") 'windower-swap-above)
  (exwm-input-set-key (kbd "s-L") 'windower-swap-right))

;; The following can only apply to EXWM buffers, else it could have unexpected effects.
(push ?\s-  exwm-input-prefix-keys)
(define-key exwm-mode-map (kbd "s-SPC") #'exwm-floating-toggle-floating)

(exwm-input-set-key (kbd "s-i") #'follow-delete-other-windows-and-split)
(exwm-input-set-key (kbd "s-O") #'exwm-layout-toggle-fullscreen)

(with-eval-after-load 'helm
  ;; Need `with-eval-after-load' here since 'helm-map is not defined in 'helm-config.
  (ambrevar/define-keys helm-map
                        "s-\\" 'helm-toggle-resplit-and-swap-windows)
  (ambrevar/exwm-global-set-key "s-c" #'helm-resume)
  (ambrevar/exwm-global-set-key "s-b" #'helm-mini)
  (ambrevar/exwm-global-set-key "s-f" #'helm-find-files)
  (ambrevar/exwm-global-set-key "s-F" #'helm-locate)
  (when (fboundp 'ambrevar/helm-locate-meta)
    (ambrevar/exwm-global-set-key "s-F" #'ambrevar/helm-locate-meta))
  (ambrevar/exwm-global-set-key "s-g" 'ambrevar/helm-grep-git-or-ag)
  (ambrevar/exwm-global-set-key "s-G" 'ambrevar/helm-grep-git-all-or-ag)
  ;; Launcher
  (ambrevar/exwm-global-set-key "s-r" 'helm-run-external-command))

(when (require 'evil nil t)
  (exwm-input-set-key (kbd "s-<tab>") #'evil-switch-to-windows-last-buffer)
  (exwm-input-set-key (kbd "C-6") #'evil-switch-to-windows-last-buffer))

;;; Emacs mode shortcuts.
(autoload 'ambrevar/helm-selector-sly "init-sly")
(if (not (require 'helm-selector nil :noerror))
    (progn
      (exwm-input-set-key (kbd "s-t") (lambda ()
                                        (interactive)
                                        (find-file (car org-agenda-files))))
      (exwm-input-set-key (kbd "s-<return>") 'eshell)
      (exwm-input-set-key (kbd "s-m") 'notmuch-hello)
      (exwm-input-set-key (kbd "s-n") 'elfeed)
      (exwm-input-set-key (kbd "s-e") 'eww))
  (ambrevar/exwm-global-set-key "s-t" 'helm-selector-org)
  (ambrevar/exwm-global-set-key "s-T" 'helm-selector-org-other-window)
  (ambrevar/exwm-global-set-key "s-<return>" 'ambrevar/helm-selector-sly)
  (ambrevar/exwm-global-set-key "S-s-<return>" 'ambrevar/helm-selector-sly-other-window)
  (ambrevar/exwm-global-set-key "s-m" 'helm-selector-notmuch)
  (ambrevar/exwm-global-set-key "s-M" 'helm-selector-notmuch-other-window)
  (ambrevar/exwm-global-set-key "s-n" 'helm-selector-elfeed)
  (ambrevar/exwm-global-set-key "s-N" 'helm-selector-elfeed-other-window) ; "n" for "news"
  (ambrevar/exwm-global-set-key "s-e" 'helm-selector-eww)
  (ambrevar/exwm-global-set-key "s-E" 'helm-selector-eww-other-window))

(when (fboundp 'magit-status)
  (if (require 'helm-selector-magit nil :noerror)
      (progn
        (ambrevar/exwm-global-set-key "s-v" #'helm-selector-magit)
        (exwm-input-set-key (kbd "s-V") #'magit-status))
    (exwm-input-set-key (kbd "s-v") #'magit-status)))

(cond
 ((executable-find "strawberry")
  (defun ambrevar/exwm-strawberry ()
    (interactive)
    (helm-exwm-switch "strawberry"))
  (defun ambrevar/exwm-strawberry-other-window ()
    (interactive)
    (helm-exwm-switch "strawberry" nil :other-window))
  (ambrevar/exwm-global-set-key "s-a" #'ambrevar/exwm-strawberry)
  (ambrevar/exwm-global-set-key "s-A" #'ambrevar/exwm-strawberry-other-window))

 ((fboundp 'emms-all)
  (exwm-input-set-key (kbd "s-a") #'emms-smart-browse)
  (exwm-input-set-key (kbd "S-s-<kp-enter>") #'emms-pause)
  (if (fboundp 'helm-emms)
      (ambrevar/exwm-global-set-key "s-A" #'helm-emms)
    (exwm-input-set-key (kbd "s-A") #'emms))))

(when (fboundp 'helm-pass)
  (ambrevar/exwm-global-set-key "s-p" #'helm-pass))

(autoload 'ambrevar/slime-to-repl "lisp")
(autoload 'ambrevar/helm-selector-sly-non-ambrevar "init-sly")
(ambrevar/exwm-global-set-key "s-<backspace>" #'ambrevar/helm-selector-sly-non-ambrevar)
;; TODO: Apparently, S-s-<backspace> is not recognized.
(ambrevar/exwm-global-set-key "M-s-<backspace>" #'ambrevar/helm-selector-sly-non-ambrevar-other-window)
(defun ambrevar/repl-switcher ()
  "Switch between Geiser and SLIME REPLs."
  (interactive)
  (pcase
      (completing-read "Lisp: " '(cider geiser slime sly racket))
    ("cider"
     (ambrevar/exwm-global-set-key "s-<backspace>" 'helm-selector-cider)
     (ambrevar/exwm-global-set-key "M-s-<backspace>" 'helm-selector-cider-other-window))
    ("geiser"
     (autoload 'helm-geiser-repl-switch "init-scheme")
     (ambrevar/exwm-global-set-key "s-<backspace>" 'helm-selector-geiser)
     (ambrevar/exwm-global-set-key "M-s-<backspace>" 'helm-selector-geiser-other-window))
    ("slime"
     (ambrevar/exwm-global-set-key "s-<backspace>" 'helm-selector-slime)
     (ambrevar/exwm-global-set-key "M-s-<backspace>" 'helm-selector-slime-other-window))
    ("sly"
     (ambrevar/exwm-global-set-key "s-<backspace>" 'helm-selector-sly)
     (ambrevar/exwm-global-set-key "<M-s-backspace>" 'helm-selector-sly-other-window))
    ("racket"
     (ambrevar/exwm-global-set-key "s-<backspace>" #'racket-repl))))
(exwm-input-set-key (kbd "s-C-<backspace>") #'ambrevar/repl-switcher)

;;; External application shortcuts.
(defun ambrevar/exwm-start (command)
  (interactive (list (read-shell-command "$ ")))
  (start-process-shell-command command nil command))
(exwm-input-set-key (kbd "s-&") #'ambrevar/exwm-start)

(when (require 'helm-exwm nil t)
  (add-to-list 'helm-source-names-using-follow "EXWM buffers")
  (setq helm-exwm-emacs-buffers-source (helm-exwm-build-emacs-buffers-source))
  (setq helm-exwm-source (helm-exwm-build-source))
  (setq helm-mini-default-sources `(helm-exwm-emacs-buffers-source
                                    helm-exwm-source
                                    helm-source-recentf
                                    ,(when (boundp 'helm-source-ls-git) 'helm-source-ls-git)
                                    helm-source-bookmarks
                                    helm-source-bookmark-set
                                    helm-source-buffer-not-found))
  (ambrevar/define-keys
   helm-exwm-map
   "M-d" 'helm-buffer-run-kill-persistent
   "S-<return>" 'helm-buffer-switch-other-window)
  ;; Web browser
  (ambrevar/exwm-global-set-key "s-w" #'helm-exwm-switch-browser)
  (ambrevar/exwm-global-set-key "s-W" #'helm-exwm-switch-browser-other-window))

(when (require 'desktop-environment nil 'noerror)
  ;; REVIEW: Remove the override on next version release:
  ;; https://gitlab.petton.fr/DamienCassou/desktop-environment
  (defun ambrevar/desktop-environment-lock-screen ()
    "Lock the screen, preventing anyone without a password from using the system."
    (interactive)
    ;; Run command asynchronously so that Emacs does not wait in the background.
    (start-process-shell-command "lock" nil desktop-environment-screenlock-command))
  (advice-add #'desktop-environment-lock-screen :override #'ambrevar/desktop-environment-lock-screen)
  (setq desktop-environment-screenshot-directory "~/Downloads")
  (define-key desktop-environment-mode-map (kbd "s-z") #'desktop-environment-lock-screen)
  ;; Re-set s-l to navigate windows.
  (define-key desktop-environment-mode-map (kbd "s-l") #'windmove-right)
  (desktop-environment-mode))

(defun ambrevar/suspend-to-sleep ()
  (interactive)
  (require 'recentf)
  (recentf-save-list)
  (call-process "loginctl" nil nil nil "suspend"))
(exwm-input-set-key (kbd "s-Z") #'ambrevar/suspend-to-sleep)

;;; Volume control
(when (require 'pulseaudio-control nil t)
  (exwm-input-set-key (kbd "s-<kp-subtract>") #'pulseaudio-control-decrease-volume)
  (exwm-input-set-key (kbd "s-<kp-add>") #'pulseaudio-control-increase-volume)
  (exwm-input-set-key (kbd "s-<kp-enter>") #'pulseaudio-control-toggle-current-sink-mute)
  (exwm-input-set-key (kbd "s--") #'pulseaudio-control-decrease-volume)
  (exwm-input-set-key (kbd "s-=") #'pulseaudio-control-increase-volume)
  (exwm-input-set-key (kbd "s-0") #'pulseaudio-control-toggle-current-sink-mute))

;;; Check for start-up errors. See ~/.profile.
(let ((error-logs (directory-files "~" t "errors.*log$")))
  (when error-logs
    (warn "Error during system startup.  See %s." (mapconcat 'identity error-logs ", "))
    (when (daemonp)
      ;; Non-daemon Emacs already brings up the *Warning* buffer.
      (setq initial-buffer-choice
            (lambda () (get-buffer "*Warnings*"))))))

;;; Some programs such as 'emacs' are better off being started in char-mode.
(defun ambrevar/exwm-start-in-char-mode ()
  (when (string-prefix-p "emacs" exwm-instance-name)
    (exwm-input-release-keyboard (exwm--buffer->id (window-buffer)))))
(add-hook 'exwm-manage-finish-hook 'ambrevar/exwm-start-in-char-mode)

;;; Some programs escape EXWM control and need be tamed.  See
;; https://github.com/ch11ng/exwm/issues/287
(add-to-list 'exwm-manage-configurations '((string= exwm-title "Kingdom Come: Deliverance") managed t))

;; Gens
(add-to-list 'exwm-manage-configurations '((string= exwm-class-name "Gens") floating nil))

;; Askpass.
(add-to-list 'exwm-manage-configurations '((string= exwm-class-name "lxqt-openssh-askpass") floating t))

(defvar ambrevar/exwm-change-screen-turn-off-primary nil
  "Turn off primary display when cable is plugged.")
;; Function to automatically toggle between internal/external screens.
(defun ambrevar/exwm-change-screen-hook ()
  (let* ((xrandr-output-regexp "\n\\([^ ]+\\) connected ")
         (xrandr-monitor-regexp "\n .* \\([^ \n]+\\)")
         (find-first-output (lambda ()
                              (call-process "xrandr" nil t nil)
                              (goto-char (point-min))
                              (re-search-forward xrandr-output-regexp nil 'noerror)
                              (match-string 1)))
         (default-output (with-temp-buffer
                           (funcall find-first-output)))
         (second-output (with-temp-buffer
                          (funcall find-first-output)
                          (forward-line)
                          (when (re-search-forward xrandr-output-regexp nil 'noerror)
                            (match-string 1)))))
    (if (not second-output)
        (progn
          (call-process "xrandr" nil nil nil "--output" default-output "--auto" "--primary")
          (with-temp-buffer
            ;; Turn off all monitors that are not DEFAULT-OUTPUT.
            ;; See "--auto" in xrandr(1) and https://github.com/ch11ng/exwm/issues/529.
            (call-process "xrandr" nil t nil "--listactivemonitors")
            (goto-char (point-min))
            (while (not (eobp))
              (when (and (re-search-forward xrandr-monitor-regexp nil 'noerror)
                         (not (string= (match-string 1) default-output)))
                (call-process "xrandr" nil nil nil "--output" (match-string 1) "--auto")))))
      (apply #'call-process
             "xrandr" nil nil nil
             "--output" second-output
             "--auto"
             (append
              (if ambrevar/exwm-change-screen-turn-off-primary '("--primary") '())
              (when ambrevar/exwm-change-screen-turn-off-primary
                (list "--output" default-output "--off"))))
      (setq exwm-randr-workspace-monitor-plist (list 0 second-output)))))

(require 'exwm-randr)
(exwm-randr-enable)

(defun ambrevar/exwm-change-screen-toggle (&optional setting)
  "Toggle automatic multiscreen configuration.
Turn off if you want to set the display settings manually.
WIth SETTING to :ENABLE or :DISABLE, set"
  (interactive)
  (if (member 'ambrevar/exwm-change-screen-hook exwm-randr-screen-change-hook)
      (progn
        (remove-hook 'exwm-randr-screen-change-hook 'ambrevar/exwm-change-screen-hook)
        (message "Manual multiscreen handling."))
    (add-hook 'exwm-randr-screen-change-hook 'ambrevar/exwm-change-screen-hook)
    (message "Automatic multiscreen handling.")))
(add-hook 'exwm-randr-screen-change-hook 'ambrevar/exwm-change-screen-hook)
;; TODO: Turn the toggle into a global minor mode.
;; Even better: Use autorandr_launcher (https://github.com/phillipberndt/autorandr/issues/210).

;; Don't intercept "C-c" since it frequently used to copy text.
;; EXWM local key bindings won't be availble then.
;; See https://github.com/ch11ng/exwm/wiki#local-key-bindings.
(define-key exwm-mode-map (kbd "C-c") nil)

(setq exwm-edit-bind-default-keys nil)
;; REVIEW: The following prevents passing "C-c" to the child window.
;; (when (require 'exwm-edit nil 'noerror)
;;   (exwm-input-set-key (kbd "C-c '") #'exwm-edit--compose)
;;   (exwm-input-set-key (kbd "C-c C-'") #'exwm-edit--compose))

(provide 'init-exwm)
