;; Auto-Insert stuff -- Guillaume Papin
;; usage:
;; (require 'sarcasm-insert)

;; (require 'autoinsert)
(auto-insert-mode 1)

;; Auto-insert Stuff
(setq auto-insert-directory (concat *sarcasm-directory*
                                    (file-name-as-directory "insert")))

;; If you don't want to be prompted before insertion
;; (setq-default auto-insert-query nil)

;; List of associated file with extension
(setq sarcasm-auto-insert-alist
      '(
        ;; file pattern . ["filename-to-insert" insertion-function]
        ;; or
        ;; (file pattern . description) . action (see `auto-insert-alist').
        (("\\.[hH]\\(h\\|pp\\)?$" . "C / C++ header") . sarcasm-generate-include-guard)
        ;;TODO: a function who ask for the kind of project, C/C++ library...
        (("\\.eproject" . "project configuration file") . "dot.eproject")
        (("Makefile" . "Makefile") . "config.mk")))

;; Add `sarcasm-auto-insert-alist' in `auto-insert-alist'.
(dolist (elem sarcasm-auto-insert-alist)
  (add-to-list 'auto-insert-alist elem))

(defun sarcasm-format-include-guard-fallback ()
  "Generate an include guard string with a project and subproject
name if `sarcasm-project-name' and `sarcasm-sub-project-name' are
defined.

note: if `sarcasm-project-name' isn't defined
`sarcasm-sub-project-name' isn't checked.

example (in .dir-locals.el file):
  ((nil
    (sarcasm-project-name . \"project-name\"))
   (\"server/directory\"
    (nil
     (sarcasm-sub-project-name . \"server\"))))"
  (let ((filename (file-name-nondirectory (file-name-sans-extension buffer-file-name)))
        (ext (file-name-extension buffer-file-name))
        (prefix (if (and (boundp 'sarcasm-project-name)
                         (stringp sarcasm-project-name))
                    (if (and (boundp 'sarcasm-sub-project-name)
                             (stringp sarcasm-sub-project-name))
                        (concat sarcasm-project-name "_"
                                sarcasm-sub-project-name "_")
                      (concat sarcasm-project-name "_"))
                  "")))
    (replace-regexp-in-string "[^A-Z0-9_]" "_"
                              (upcase (concat prefix filename "_" ext)))))

(defun sarcasm-format-include-guard ()
  "If not in a project (see `eproject-mode') use
  `sarcasm-format-include-guard-fallback'.

Format an include guard, in 2 parts:
        - the project name
        - the filename mapping (see `eproject--shorten-filename')

example, with the following information:
  project name:     my server
  shorten filename: utils/pthread/mutex.hh

the result will be: _MY_SERVER_UTILS_PTHREAD_MUTEX_HH_

As said here: http://en.wikibooks.org/wiki/More_C%2B%2B_Idioms/Include_Guard_Macro
don't add an underscore at the begining of the define.

> Programmers often have their include guard macros start with
  one or more underscores, followed by uppercase letters, even
  though such identifiers are officially reserved for the
  implementation of the compiler and the Standard Library,
  according to the C++ Standard (ISO/IEC 14882:2003).
"
  (if (and (featurep 'eproject) eproject-mode)
      (let ((filename (car (eproject--shorten-filename buffer-file-name))))
        (replace-regexp-in-string "[^A-Z0-9_]" "_"
                                  (upcase (concat (eproject-name) "_" filename "_"))))
    (sarcasm-format-include-guard-fallback)))

(defun sarcasm-generate-include-guard ()
  "Generate an include guard (should be in a C/C++ file), used by
`auto-insert-mode'."
  (insert "guard")
  (yas/expand))

(provide 'sarcasm-insert)
