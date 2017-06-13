(require 'bind-key)

(defun sarcasm-truncate-lines-on ()
  (toggle-truncate-lines 1))

(use-package compile
  :defer t
  :config
  (setq compilation-scroll-output 'first-error)
  (add-hook 'compilation-mode-hook 'sarcasm-truncate-lines-on))

(defun sarcasm-compile (&optional should-compile)
  "Run the same compile as the last time.

If there was no last time, or there is a prefix argument,
this acts like `compile'."
  (interactive "P")
  (if (and (not should-compile)
           (boundp 'compilation-last-buffer)
           compilation-last-buffer)
      (progn
        (set-buffer compilation-last-buffer)
        (revert-buffer t t))
    (call-interactively #'compile)))

(bind-keys ("C-c c" . sarcasm-compile)
           ("M-n" . next-error)
           ("M-p" . previous-error))
