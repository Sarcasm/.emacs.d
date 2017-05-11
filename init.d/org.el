(add-to-list 'auto-mode-alist '("TODO" . org-mode))

(use-package org
  :init
  (setq-default org-log-done t
                org-src-fontify-natively t ;display specific mode colors in src block
                org-insert-mode-line-in-empty-file t
                org-hide-emphasis-markers t
                org-hide-leading-stars t)
  :bind (("C-c o a" . org-agenda)
         ("C-c o l" . org-store-link)
         ("C-c o o" . org-open-at-point-global)
         ("C-c o i" . org-insert-link-global))
  :config
  ;; Make windmove work in Org-Mode:
  (add-hook 'org-shiftup-final-hook    'windmove-up)
  (add-hook 'org-shiftleft-final-hook  'windmove-left)
  (add-hook 'org-shiftdown-final-hook  'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right))
