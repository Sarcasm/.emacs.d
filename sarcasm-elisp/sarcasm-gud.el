;; Debugger settings -- Guillaume Papin
;; usage:
;; (require 'sarcasm-gud)

;; Use gdb-many-windows by default
(setq gdb-many-windows t)

(defun sarcasm-gud-hooks ()
  (gud-tooltip-mode 1))

(add-hook 'gud-mode-hook 'sarcasm-gud-hooks)

(provide 'sarcasm-gud)
