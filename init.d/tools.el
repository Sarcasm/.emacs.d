(use-package ledger-mode
  :ensure t
  :defer t
  :config
  (use-package sarcasm-ledger
    :bind ("C-c ." . sarcasm-ledger-time-stamp)))

(use-package ag
  :ensure t
  :defer t
  :config (setq ag-highlight-search t
                ag-reuse-buffers t))
