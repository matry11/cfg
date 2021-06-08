;;; C/C++

(dolist (map (list c-mode-map c++-mode-map))
  (ambrevar/define-keys map "C-c m" 'cc-main
                        "<f5>" 'ambrevar/cc-clean
                        "M-." 'semantic-ia-fast-jump
                        "C-c C-d" 'semantic-ia-show-doc
                        "M-<tab>" 'semantic-complete-analyze-inline)
  (when (require 'company nil t)
    (define-key map (kbd "M-<tab>") (if (require 'helm-company nil t) 'helm-company 'company-complete)))
  (when (require 'gtk-look nil 'noerror)
    (define-key map (kbd "C-c d") 'gtk-lookup-symbol)))
;; (define-key map (kbd "C-c o") 'ff-find-other-file)

(when (and (require 'gtk-look nil 'noerror)
           (require 'w3m nil 'noerror))
  ;; Browse GTK documentation within Emacs.
  ;; TODO: Use eww instead of w3m.  For some reason it hangs for almost a minute
  ;; with eww.
  (add-to-list 'browse-url-browser-function '("file:///.*/gtk-doc/html/.*" . w3m-browse-url)))

(with-no-warnings (defvaralias 'c-basic-offset 'tab-width))

;;; C additional faces.
;;; Useless in quasi-monochrome.
;; (dolist (mode '(c-mode c++-mode))
;;   (font-lock-add-keywords
;;    mode
;;    '(("\\<\\(and\\|or\\|not\\)\\>" . font-lock-keyword-face)
;;      ;; Functions.
;;      ("\\<\\(\\sw+\\)(" 1 'font-lock-function-name-face)
;;      ("\\<\\(\\sw+\\)<\\sw+>(" 1 'font-lock-function-name-face))))

(defvar-local ambrevar/cc-ldlibs "-lm -pthread"
  "Custom linker flags for C/C++ linkage.")

(defvar-local ambrevar/cc-ldflags ""
  "Custom linker libs for C/C++ linkage.")

(defun ambrevar/cc-set-compiler (&optional nomakefile)
  "Set compile command to be nearest Makefile or a generic command.
The Makefile is looked up in parent folders. If no Makefile is
found (or if NOMAKEFILE is non-nil or if function was called with
universal argument), then a configurable commandline is
provided."
  (interactive "P")
  (hack-local-variables)
  ;; Alternatively, if a Makefile is found, we could change default directory
  ;; and leave the compile command to "make".  Changing `default-directory'
  ;; could have side effects though.
  (let ((makefile-dir (locate-dominating-file "." "Makefile")))
    (if (and makefile-dir (not nomakefile))
        (setq compile-command (concat "make -k -C " (shell-quote-argument (file-name-directory makefile-dir))))
      (setq compile-command
            (let
                ((c++-p (eq major-mode 'c++-mode))
                 (file (file-name-nondirectory buffer-file-name)))
              (format "%s %s -o '%s' %s %s %s"
                      (if c++-p
                          (or (getenv "CXX") "g++")
                        (or (getenv "CC") "gcc"))
                      (shell-quote-argument file)
                      (shell-quote-argument (file-name-sans-extension file))
                      (if c++-p
                          (or (getenv "CXXFLAGS") "-Wall -Wextra -Wshadow -DDEBUG=9 -g3 -O0")
                        (or (getenv "CFLAGS") "-ansi -pedantic -std=c11 -Wall -Wextra -Wshadow -DDEBUG=9 -g3 -O0"))
                      (or (getenv "LDFLAGS") ambrevar/cc-ldflags)
                      (or (getenv "LDLIBS") ambrevar/cc-ldlibs)))))))

(defun ambrevar/cc-clean ()
  "Find Makefile and call the `clean' rule. If no Makefile is
found, no action is taken. The previous `compile' command is
restored."
  (interactive)
  (let (compile-command
        (makefile-dir (locate-dominating-file "." "Makefile")))
    (when makefile-dir
      (compile (format "make -k -C %s clean" (shell-quote-argument makefile-dir))))))

(defun ambrevar/cc-force-compile ()
  "If current `compile-command' is `make', force compilation.
This is done by appending the -B flag (--always-make) temporarily.
If the command is not `make', run it normally. "
  (interactive)
  (let ((compile-command compile-command))
    (if (string-prefix-p "make " compile-command)
        (compile (format "%s -B" compile-command))
      (recompile))))

;; TODO: See https://github.com/koko1000ban/emacs-uncrustify-mode.
(defun ambrevar/cc-format-with-uncrustify (&optional cfg-file start end)
  "Run uncrustify(1) on current buffer or region."
  (interactive "f\nr")
  (let (status
        (start (or (and (region-active-p) start)
                   (point-min)))
        (end (or (and (region-active-p) end)
                 (point-max)))
        (formatbuf (get-buffer-create " *C format buffer*"))
        (stderr (make-temp-file "uncrustify")))
    (setq status
          (call-process-region start end
                               "uncrustify"
                               nil (list formatbuf stderr) nil
                               ;; "-lc" ; Uncrustify should be able to auto-detect.
                               "-c"
                               (or cfg-file
                                   (expand-file-name ".uncrustify.cfg" (getenv "HOME")))))
    (if (/= status 0)
        (let ((error-message
               (with-temp-buffer
                 (insert-file-contents-literally stderr)
                 (buffer-string))))
          (delete-file stderr)
          (kill-buffer formatbuf)
          (error "error running uncrustify: %s" error-message))
      (delete-file stderr)
      (let ((old-line (line-number-at-pos))
            (old-column (current-column))
            (old-window-start-line (- (line-number-at-pos)
                                      (line-number-at-pos (window-start)))))
        (buffer-disable-undo)
        (save-mark-and-excursion
          (delete-region (or start (point-min)) (or end (point-max)))
          (insert-buffer-substring formatbuf))
        (goto-line old-line)
        (move-to-column old-column)
        (ignore-errors
          ;; recenter won't work if selected window is not the target buffer.
          (recenter old-window-start-line))
        (buffer-enable-undo))
      (kill-buffer formatbuf)))
  ;; Return nil if in a `write-file-functions'.
  nil)

(defun ambrevar/cc-format-file-lookup ()
  "Find .clang-format or .uncrustify.cfg in parent folder up to Git root.
Return nil if non is found or if not a Git repository."
  (unless (require 'magit nil 'noerror)
    (error "Magit is missing"))
  (when (or (magit-get-current-branch) (magit-get-current-tag))
    (let ((git-root (magit-rev-parse "--show-toplevel"))
          (default-directory default-directory))
      (while (and (string= (magit-rev-parse "--show-toplevel") git-root)
                  (not (file-exists-p ".clang-format"))
                  (not (file-exists-p ".uncrustify.cfg")))
        (cd ".."))
      (or (and (file-exists-p ".clang-format")
               (expand-file-name ".clang-format" default-directory))
          (and (file-exists-p ".uncrustify.cfg")
               (expand-file-name ".uncrustify.cfg" default-directory))))))

(defun ambrevar/cc-format ()
  "Format C file.
It uses `ambrevar/cc-format-file-lookup' to find the format rules.
This is suitable for a `before-save-hook'."
  (interactive)
  (let ((cfg-file (ambrevar/cc-format-file-lookup)))
    (cond
     ((and (string= (file-name-base cfg-file) ".clang-format")
           (require 'clang-format nil 'noerror))
      (clang-format-buffer))
     ((and (string= (file-name-base cfg-file) ".uncrustify")
           (executable-find "uncrustify"))
      (ambrevar/cc-format-with-uncrustify cfg-file)))))

(defun ambrevar/cc-turn-on-format ()
  "Add `ambrevar/cc-format' to `before-save-hook' locally.
You can add your mode hook (e.g. `c-mode-hook')."
  (add-hook 'write-file-functions 'ambrevar/cc-format nil 'local))

;;; GMP documentation
(with-eval-after-load "info-look"
  (let ((mode-value (assoc 'c-mode (assoc 'symbol info-lookup-alist))))
    (setcar (nthcdr 3 mode-value)
            (cons '("(gmp)Function Index" nil "^ -.* " "\\>")
                  (nth 3 mode-value)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Options

;; We don't want semantic in Scheme and others.
(setq semantic-new-buffer-setup-functions
      '((c-mode . semantic-default-c-setup)
        (c++-mode . semantic-default-c-setup)))
;;; Make sure Semanticdb folders is set before starting semantic.
(semantic-mode 1)
;; (global-semantic-stickyfunc-mode)
;; (global-semantic-decoration-mode)
;; Eldoc does not work but there is Semantic.
(global-semantic-idle-summary-mode)
(setq semantic-idle-scheduler-idle-time 5)

;; Add support for some subdir-includes.
;; TODO: This could be automated by parsing `pkg-config --list-all` or maybe
;; `find -L .  -maxdepth 1 -type d`.
(setq semantic-c-dependency-system-include-path
      (append (mapcar (lambda (lib)
                        (expand-file-name
                         lib
                         (or (getenv "CPATH")
                             (if (file-directory-p (expand-file-name "~/.guix-profile/include"))
                                 "~/.guix-profile/include"
                               "/usr/include"))))
                      '("glib-2.0" "gtk-3.0" "webkitgtk-4.0"))
              '("/usr/include")))

(c-add-style
 "ambrevar"
 `((c-comment-only-line-offset . 0)
   (c-auto-align-backslashes . nil)
   (c-basic-offset . ,tab-width)
   (c-offsets-alist
    (arglist-cont-nonempty . +)
    (arglist-intro . +)
    (c . 0)
    (case-label . 0)
    (cpp-define-intro . 0)
    (cpp-macro . 0)
    (knr-argdecl-intro . 0)
    (label . 0)
    (statement-block-intro . +)
    (statement-cont . +)
    (substatement-label . 0)
    (substatement-open . 0))))
(nconc c-default-style '((c-mode . "ambrevar") (c++-mode . "ambrevar")))

;;; Note that in Emacs 24, cc-mode calls its hooks manually in each mode init
;;; function. Since cc modes belong to prog-mode, each hook is called another
;;; time at the end of the initialization. No big deal since we only set some
;;; variables.
(dolist (hook '(c-mode-hook c++-mode-hook))
  (when (require 'company nil t)
    (add-hook hook 'company-mode))
  (add-hook hook 'ambrevar/cc-turn-on-format)
  (add-hook hook 'ambrevar/cc-set-compiler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Skeletons

;;; Note that it is possible to extend the skel syntax with
;;; `skeleton-further-elements'. For instance:
                                        ; (setq skeleton-further-elements '((q "\"")))

(define-skeleton cc-debug
  "Insert debug macros."
  nil
  > "#ifdef DEBUG
#define DEBUG_CMD(CMD) do {CMD;} while(0)
#else
#define DEBUG_CMD(CMD) do {} while(0)
#endif

"
  '(insert-and-indent
    "#define DEBUG_PRINT(...) DEBUG_CMD( \\
fprintf(stderr, \"%s:%d:\\t(%s)\\t\", __FILE__, __LINE__, __func__); \\
fprintf(stderr, __VA_ARGS__); \\
fprintf(stderr, \"\\n\"); \\
)"))

(define-skeleton cc-getopt
  "Insert a getopt template."
  nil
  > "int opt;" \n
  "while ((opt = getopt(argc, argv, \":hV\")) != -1) {" \n
  "switch(opt) {" \n
  "case 'h':" > \n
  "usage(argv[0]);" \n
  "return 0;" \n
  "case 'V':" > \n
  "version();" \n
  "return 0;" \n
  "case ':':" > \n
  "fprintf(stderr, \"ERROR: -%c needs an argument.\\nTry '%s -h' for more information.\\n\", optopt, argv[0]);" \n
  "return EXIT_FAILURE;" \n
  "case '?':" > \n
  "fprintf(stderr, \"ERROR: Unknown argument %c.\\nTry '%s -h' for more information.\\n\", optopt, argv[0]);" \n
  "return EXIT_FAILURE;" \n
  "default:" > \n
  "usage(argv[0]);" \n
  "return EXIT_SUCCESS;" \n
  "}" > \n
  "}" > "\n" \n
  "if (optind >= argc) {" \n
  "fprintf(stderr, \"Expected argument after options\\n\");" \n
  "exit(EXIT_FAILURE);" \n
  "}" > \n)

(define-skeleton cc-loadfile
  "Insert loadfile function."
  nil
  "unsigned long loadfile(const char *path, char **buffer_ptr) {" \n
  "#define MAX_FILESIZE 1073741824 /* One gigabyte */" > "\n" \n
  "/* Handle variable. */" \n
  "char *buffer;" "\n" \n
  "FILE *file = fopen(path, \"rb\");" \n
  "if (file == NULL) {" \n
  "perror(path);" \n
  "return 0;" \n
  "}" > "\n" \n
  "fseek(file, 0, SEEK_END);" \n
  "long length = ftell(file);" \n
  "/* fprintf(stdout, \"Note: file %s is %u bytes long.\\n\", path, length); */" "\n" \n
  "if (length > MAX_FILESIZE) {" \n
  "fprintf(stderr, \"%s size %ld is bigger than %d bytes.\\n\", path, length, MAX_FILESIZE);" \n
  "fclose(file);" \n
  "return 0;" \n
  "}" > "\n" \n
  "fseek(file, 0, SEEK_SET);" \n
  "buffer = (char *)malloc(length + 1);" \n
  "if (buffer == NULL) {" \n
  "perror(\"malloc\");" \n
  "fclose(file);" \n
  "return 0;" \n
  "}" > "\n" \n
  "if (fread(buffer, 1, length, file) == 0) {" \n
  "fclose(file);" \n
  "return 0;" \n
  "}" > "\n" \n
  "buffer[length] = '\\0';" \n
  "fclose(file);" "\n" \n
  "*buffer_ptr = buffer;" \n
  "return length;" \n
  "}" > \n)

(define-skeleton cc-main
  "Insert main function with basic includes."
  nil
  > "#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int main(int argc, char **argv) {" \n
  > @ _ \n
  > "return 0;
}" \n)

(define-skeleton cc-usage-version
  "Insert usage() and version() functions."
  "Synopsis: "
  > "static void usage(const char *executable) {" \n
  "printf(\"Usage: %s [OPTIONS]\\n\\n\", executable);" \n
  "puts(\"" str "\\n\");" "\n" \n

  "puts(\"Options:\");" \n
  "puts(\"  -h        Print this help.\");" \n
  "puts(\"  -V        Print version information.\");" "\n" \n

  "puts(\"\");" \n
  "printf(\"See %s for more information.\\n\", MANPAGE);" \n
  "}" > "\n" \n

  "static void version() {" \n
  "printf(\"%s %s\\n\", APPNAME, VERSION);" \n
  "printf(\"Copyright © %s %s\\n\", YEAR, AUTHOR);" \n
  "}" > \n)

(provide 'init-cc)
