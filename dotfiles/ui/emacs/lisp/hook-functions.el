(defun ambrevar/turn-on-column-number-mode ()
  "Unconditionally turn on `column-number-mode' for the current buffer."
  (set (make-variable-buffer-local 'column-number-mode) t))

(defun ambrevar/turn-on-complete-filename ()
  "Unconditionally turn on `comint-dynamic-complete-filename' for the current buffer."
  (add-to-list 'completion-at-point-functions 'comint-dynamic-complete-filename t))

(defun ambrevar/turn-off-indent-tabs ()
  "Unconditionally turn off tab indentation."
  (setq indent-tabs-mode nil))

(defun ambrevar/turn-on-indent-tabs ()
  "Unconditionally turn on tab indentation."
  (setq indent-tabs-mode t))

(defun ambrevar/turn-off-line-number-mode ()
  "Unconditionally turn off `line-number-mode' fur the current buffer.."
  (set (make-variable-buffer-local 'line-number-mode) nil))

(defun ambrevar/turn-on-tab-width-to-2 ()
  "Unconditionally set tab width to 2."
  (setq tab-width 2))

(defun ambrevar/turn-on-tab-width-to-4 ()
  "Unconditionally set tab width to 4."
  (setq tab-width 4))

(defun ambrevar/turn-on-tab-width-to-8 ()
  "Unconditionally set tab width to 8."
  (setq tab-width 8))

(provide 'hook-functions)
