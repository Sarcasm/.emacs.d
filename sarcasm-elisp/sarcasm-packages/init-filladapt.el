(defun sarcasm-setup-filladapt ()
  (require 'filladapt)

  (c-setup-filladapt)
  (auto-fill-mode 1)
  (filladapt-mode 1))

(add-hook 'c-mode-common-hook 'sarcasm-setup-filladapt)
