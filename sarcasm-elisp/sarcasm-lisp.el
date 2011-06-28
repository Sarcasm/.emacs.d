;; Lisp stuff -- Guillaume Papin
;; usage:
;; (require 'sarcasm-lisp)

(defun sarcasm-lisp-mode-hooks ()
  (turn-on-eldoc-mode)

  (font-lock-add-keywords nil
                          '(("\\<\\(FIXME\\):" 1 font-lock-warning-face t)))
  (font-lock-add-keywords nil
                          '(("\\<\\(TODO\\):" 1 font-lock-keyword-face t)))
  )

(add-hook 'emacs-lisp-mode-hook 'sarcasm-lisp-mode-hooks)
(add-hook 'lisp-interaction-mode-hook 'sarcasm-lisp-mode-hooks)
(add-hook 'ielm-mode-hook 'sarcasm-lisp-mode-hooks)

(provide 'sarcasm-lisp)
