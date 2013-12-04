;; Javascript stuff -- Guillaume Papin
;; usage:
;; (require 'sarcasm-js)

(defun sarcasm-js-hooks ()
  (interactive)
  (electric-layout-mode -1)
  (setq comment-auto-fill-only-comments t)
  (auto-fill-mode 1)
  (filladapt-mode 1))

(add-hook 'javascript-mode-hook 'sarcasm-js-hooks)
(add-hook 'js-mode-hook 'sarcasm-js-hooks)
(add-hook 'js2-mode-hook 'sarcasm-js-hooks)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(when (require 'js2-refactor nil t)
  (js2r-add-keybindings-with-prefix "C-c C-m"))

(provide 'sarcasm-js)
