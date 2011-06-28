;; Ruby stuff -- Guillaume Papin
;; usage:
;; (require 'sarcasm-ruby)

(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))

(defun sarcasm-ruby-mode-hook ()
  "Additionnal ruby mode settings."
  (auto-fill-mode 1)
  (fixme-and-todo-font-lock)
)

(add-hook 'ruby-mode-hook 'sarcasm-ruby-mode-hook)

(provide 'sarcasm-ruby)
