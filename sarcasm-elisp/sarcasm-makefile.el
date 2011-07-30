;; Makefile stuff -- Guillaume Papin
;; usage:
;; (require 'sarcasm-makefile)

(defun sarcasm-makefile-mode-hook ()
  "Hooks for Makefile mode."

  (font-lock-add-keywords nil
                          '(("\\<\\(FIXME\\):" 1 font-lock-warning-face t)))
  (font-lock-add-keywords nil
                          '(("\\<\\(TODO\\):" 1 font-lock-keyword-face t)))
)

(add-hook 'makefile-mode-hook 'sarcasm-makefile-mode-hook)

(provide 'sarcasm-makefile)
