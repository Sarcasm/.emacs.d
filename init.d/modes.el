(require 'sarcasm)

(run-with-idle-timer 1 nil #'mapc #'funcall
                     '(delete-selection-mode
                       electric-layout-mode
                       electric-pair-mode
                       global-auto-revert-mode
                       global-hl-line-mode
                       savehist-mode
                       show-paren-mode
                       winner-mode))

(use-package subword
  :defer 0
  :config (sarcasm-deffered-global-mode #'global-subword-mode
                                        #'subword-mode
                                        'subword-mode))

(use-package multiple-cursors
  :ensure t
  :init (setq-default mc/list-file (etc "multiple-cursors/lists.el"))
  :bind (("C-;" . mc/mark-all-dwim)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))
