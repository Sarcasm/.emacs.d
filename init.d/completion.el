(require 'sarcasm)

(defun sarcasm-company-dabbrev-ignore-except-magit-diff (buffer)
  (let ((name (buffer-name)))
    (and (string-match-p "\\`[ *]" name)
         (not (string-match-p "\\*magit-diff:" name)))))

(defun sarcasm-git-commit-setup-hook ()
  (setq-local company-dabbrev-code-modes '(text-mode magit-diff-mode))
  (setq-local company-dabbrev-code-other-buffers 'code)
  (setq-local company-dabbrev-ignore-buffers
              #'sarcasm-company-dabbrev-ignore-except-magit-diff))

(use-package company
  :ensure t
  :defer 0
  :bind ("M-RET" . company-complete)
  :functions global-company-mode
  :preface (declare-function company-mode-on "ext:company")
  :config
  (setq company-selection-wrap-around t)
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case nil)
  (sarcasm-deffered-global-mode #'global-company-mode
                                #'company-mode-on
                                'company-mode)

  ;; When redacting commit message in magit,
  ;; with the diff view in the other window,
  ;; use company-dabbrev to complete words of the other buffer
  ;; in the commit message buffer.
  ;; Stolen from https://github.com/company-mode/company-mode/issues/704#issuecomment-325783249
  (add-hook 'git-commit-setup-hook #'sarcasm-git-commit-setup-hook))

(use-package ivy
  :ensure t
  :defer 0
  :config (ivy-mode))
