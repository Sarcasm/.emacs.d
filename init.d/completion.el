(require 'sarcasm)

(use-package company
  :ensure t
  :defer 0
  :bind ("M-RET" . company-complete)
  :functions global-company-mode
  :preface (declare-function company-mode-on "ext:company")
  :config
  (sarcasm-deffered-global-mode #'global-company-mode
                                #'company-mode-on
                                'company-mode))
