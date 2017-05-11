(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
;; When using C-x C-e to edit the command line
(add-to-list 'auto-mode-alist '("\\`/tmp/zshecl[0-9]+\\'" . sh-mode))
;; Automatically 'chmod' scripts as they are saved
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
