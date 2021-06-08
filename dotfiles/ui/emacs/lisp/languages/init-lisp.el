;;; Lisp

(require 'init-lispy)

(add-hook 'lisp-mode-hook 'ambrevar/turn-on-complete-filename)
(add-hook 'lisp-mode-hook 'ambrevar/turn-on-tab-width-to-8) ; Because some existing code uses tabs.
(add-hook 'lisp-mode-hook 'ambrevar/turn-off-indent-tabs)   ; Should not use tabs.
(add-hook 'lisp-mode-hook 'ambrevar/init-lispy)
(when (fboundp 'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode))

;; Read CLHS locally.
(or
 ;; Quicklisp package.
 (load "~/.quicklisp/clhs-use-local.el" 'noerror)
 ;; Unofficial Guix package (non-free license).
 (when (require 'clhs nil 'noerror)
   (clhs-setup)))

(with-eval-after-load 'sly
  (require 'init-sly))

(with-eval-after-load 'slime
  (require 'init-slime))

(provide 'init-lisp)
