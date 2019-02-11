(use-package ledger-mode
  :ensure t
  :defer t
  :config
  (autoload 'org-read-date "org")
  (defun ledger-read-date (prompt)
    "Return user-supplied date after `PROMPT', defaults to today."
    (let ((date (parse-time-string (org-read-date))))
      (format "%04d/%02d/%02d" (nth 5 date) (nth 4 date) (nth 3 date))))
  (use-package sarcasm-ledger
    :bind ("C-c ." . sarcasm-ledger-time-stamp)))

(use-package ag
  :ensure t
  :defer t
  :config (setq ag-highlight-search t
                ag-reuse-buffers t))
