(use-package org
  :bind (("C-c o a" . org-agenda)
         ("C-c o l" . org-store-link)
         ("C-c o o" . org-open-at-point-global)
         ("C-c o i" . org-insert-link-global))
  :mode ("TODO" . org-mode)
  :init
  (setq-default org-log-done t
                org-src-fontify-natively t ;display specific mode colors in src block
                org-insert-mode-line-in-empty-file t
                org-hide-emphasis-markers t
                org-hide-leading-stars t)
  ;; Configure `display-buffer' behaviour for some special buffers.
  ;; see http://www.lunaryorn.com/2015/04/29/the-power-of-display-buffer-alist.html
  ;; and https://github.com/lunaryorn/.emacs.d/blob/2233f7dc277453b7eaeb447b00d8cb8d72435318/init.el#L420-L439
  (add-to-list 'display-buffer-alist
               '("\\*Calendar\\*"
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.22)))
  :config
  ;; Make windmove work in Org-Mode:
  (add-hook 'org-shiftup-final-hook    'windmove-up)
  (add-hook 'org-shiftleft-final-hook  'windmove-left)
  (add-hook 'org-shiftdown-final-hook  'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right))
