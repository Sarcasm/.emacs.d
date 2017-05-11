(require 'sarcasm)

(use-package flycheck
  :ensure t
  :defer 3
  :functions global-flycheck-mode
  :preface (declare-function flycheck-mode-on-safe "ext:flycheck")
  :init (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :config (sarcasm-deffered-global-mode #'global-flycheck-mode
                                        #'flycheck-mode-on-safe
                                        'flycheck-mode))
