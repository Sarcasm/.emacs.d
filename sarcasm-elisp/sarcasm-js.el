;; Javascript stuff -- Guillaume Papin
;; usage:
;; (require 'sarcasm-js)

(defun sarcasm-electric-layout-mode-off ()
  (interactive)
  (electric-layout-mode -1))

(add-hook 'javascript-mode-hook 'sarcasm-electric-layout-mode-off)
(add-hook 'js-mode-hook 'sarcasm-electric-layout-mode-off)
