(defun sarcasm-py-yapf-default-keybindings ()
  (local-set-key (kbd "C-S-f") 'py-yapf-buffer))
(add-hook 'python-mode-hook 'sarcasm-py-yapf-default-keybindings)
