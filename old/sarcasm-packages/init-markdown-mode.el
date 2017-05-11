(defun sarcasm-markdown-mode-setup ()
  (setq show-trailing-whitespace t))

(add-hook 'markdown-mode-hook 'sarcasm-markdown-mode-setup)
