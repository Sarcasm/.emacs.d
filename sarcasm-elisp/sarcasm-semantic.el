;; General Semantic stuff -- Guillaume Papin
;; usage:
;; (require 'sarcasm-semantic)

;; Enable support for GNU Global
(require 'semantic/db-global)
(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)

(provide 'sarcasm-semantic)
