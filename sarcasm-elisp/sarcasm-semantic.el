;; General Semantic stuff -- Guillaume Papin
;; usage:
;; (require 'sarcasm-semantic)

;; Enable support for GNU Global
;; (require 'semantic/db-global)
;; (semanticdb-enable-gnu-global-databases 'c-mode)
;; (semanticdb-enable-gnu-global-databases 'c++-mode)

;; (global-semantic-idle-scheduler-mode 1)
;; ;; (global-semantic-idle-completions-mode 1)
;; (global-semantic-idle-summary-mode 1)
;; (global-semantic-decoration-mode 1)
;; (global-semantic-highlight-func-mode 1)
;; (global-semantic-highlight-edits-mode 1)
;; (global-semantic-idle-local-symbol-highlight-mode 1)

;; (require 'semantic/symref/cscope)
;; (require 'semantic/symref/global)
;; (require 'cedet-cscope)
;; (require 'cedet-global)

;; (global-ede-mode 1)

;; (defun sarcasm-semantic-for-c-mode-hook ()
;;   (define-key c-mode-base-map (kbd "C-c g") 'semantic-ia-fast-jump) ;go
;;   (define-key c-mode-base-map (kbd "C-c ?") 'semantic-ia-show-doc)  ;help/doc
;;   )

;; (add-hook 'c-mode-common-hook 'sarcasm-semantic-for-c-mode-hook)

(provide 'sarcasm-semantic)
