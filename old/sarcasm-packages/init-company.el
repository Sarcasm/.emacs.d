(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "M-RET") 'company-complete)
(setq company-selection-wrap-around t)
