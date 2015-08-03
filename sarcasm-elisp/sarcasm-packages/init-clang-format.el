(defun clang-format-default-keybindings ()
  (define-key c-mode-base-map (kbd "C-S-f") 'clang-format-region))

(add-hook 'c++-mode-hook 'clang-format-default-keybindings)
(add-hook 'c-mode-hook 'clang-format-default-keybindings)
