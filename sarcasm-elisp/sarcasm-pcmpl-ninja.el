;; Ninja targets completion for pcomplete -- Guillaume Papin
;; usage:
;; (require 'sarcasm-pcmpl-ninja)

(require 'pcomplete)

(defun pcomplete/ninja ()
  "Completion rules for the `ninja' command."
  (pcomplete-here (pcmpl-ninja-targets)))

(defun pcmpl-ninja-targets ()
  "Returns a list of targets for the current project."
  (with-temp-buffer
    (shell-command "ninja -t targets all" (current-buffer))
    (goto-char (point-min))
    (let (res)
      (while (re-search-forward "^\\([a-zA-Z0-9-_.]+\\):" nil t)
        (setq res (cons (match-string 1) res)))
      (sort res 'string-lessp))))

(provide 'sarcasm-pcmpl-ninja)
