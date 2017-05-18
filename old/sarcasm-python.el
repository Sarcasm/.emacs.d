;; Python stuff -- Guillaume Papin
;; usage:
;; (require 'sarcasm-python)

(defun sarcasm-python-hook ()
  ;; if not disabled it adds unwanted indentation too often, it's quite
  ;; annoying.
  (electric-indent-mode -1)
  ;; on the other hand `newline-and-indent' seems to offer a correct
  ;; behavior
  (local-set-key (kbd "RET") 'newline-and-indent)
  (subword-mode 1)                        ;C-c C-w to toggle
  (setq truncate-lines t))

(add-hook 'python-mode-hook 'sarcasm-python-hook)

(provide 'sarcasm-python)