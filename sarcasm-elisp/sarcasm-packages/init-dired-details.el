;; (require 'dired-details)
(require 'dired)

(define-key dired-mode-map "/" 'dired-details-toggle)
;; or to just this, if you set ‘dired-details-hidden-string’ to ""
;; instead of "[...]":
(setq dired-details-hidden-string "")
