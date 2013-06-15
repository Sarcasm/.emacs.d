(eval-when-compile (require 'jedi nil t))
(setq jedi:setup-keys t
      jedi:complete-on-dot t)

(add-hook 'python-mode-hook 'jedi:setup)
