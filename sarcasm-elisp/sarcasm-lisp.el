;; Lisp stuff -- Guillaume Papin
;; usage:
;; (require 'sarcasm-lisp)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(provide 'sarcasm-lisp)
