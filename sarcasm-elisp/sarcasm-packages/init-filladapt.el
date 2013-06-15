(require 'filladapt)

(defun sarcasm-setup-filladapt ()
  (auto-fill-mode 1)
  (filladapt-mode 1))

(add-hook 'c-mode-common-hook 'sarcasm-setup-filladapt)
(add-hook 'c-mode-common-hook 'c-setup-filladapt)
(add-hook 'python-mode-hook 'sarcasm-setup-filladapt)
(add-hook 'rst-mode-hook 'sarcasm-setup-filladapt)
