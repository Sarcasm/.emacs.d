;; Auto-Insert stuff -- Guillaume Papin
;; usage:
;; (require 'sarcasm-insert)

;; (require 'autoinsert)
(auto-insert-mode 1)

;; Auto-insert Stuff
(setq auto-insert-directory (concat (file-name-as-directory *sarcasm-load-path*)
                                    "insert/"))

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
        (("\\Makefile" . "Makefile") . "config.mk")))

;; Add `sarcasm-auto-insert-alist' in `auto-insert-alist'.
(dolist (elem sarcasm-auto-insert-alist)
  (add-to-list 'auto-insert-alist elem))

(defun sarcasm-format-include-guard ()
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
    (replace-regexp-in-string "[^A-Z_]" "_"
                              (upcase (concat "_" prefix filename "_" ext "_")))))

(defun sarcasm-generate-include-guard ()
  "Generate an include guard (should be in a C/C++ file)."
  (insert "guard")
  (yas/expand))

(provide 'sarcasm-insert)