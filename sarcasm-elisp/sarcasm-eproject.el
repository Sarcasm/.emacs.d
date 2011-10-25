;; eproject configuration -- Guillaume Papin
;; usage:
;; (require 'sarcasm-eproject)

(require 'eproject)
(require 'eproject-extras)

(defconst sarcasm-project-irrelevant-files
  (list sarcasm-ignored-files-re
        (concat "^" (regexp-opt sarcasm-ignored-files) "$"))
"List of regexp describing irrelevant files for beeing part of a
project.")

(setq eproject-completing-read-function 'eproject--ido-completing-read
      eproject-todo-expressions '("TODO" "FIXME" "DEBUG" "BUG" "XXX"))

;; List of sarcasm project types
(require 'sarcasm-cc-project)

(provide 'sarcasm-eproject)
