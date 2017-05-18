(use-package py-yapf
  :ensure t
  :after python-mode
  :defines python-mode-map
  :bind (:map python-mode-map ("C-S-f" . py-yapf-buffer)))

(setq-default anaconda-mode-installation-directory (cache "anaconda-mode"))

(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  (use-package company-anaconda
    :ensure t
    :defer t
    :after company
    :config (add-to-list 'company-backends 'company-anaconda)))
