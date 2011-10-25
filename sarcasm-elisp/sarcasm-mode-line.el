;; Mode-Line content -- Guillaume Papin
;; usage:
;; (require 'sarcasm-mode-line)
;;
;; NOTE: not sure if it's a good idea, just keep the comment here if
;;       one day I want to customize the mode line.
;;
;; M-x describe-variable `mode-line-format'
;; http://emacs-fu.blogspot.com/2011/08/customizing-mode-line.html

;; (setq-default
;;  mode-line-format
;;  (list
;;   "%e"                         ;print error message about full memory
;;   (:eval
;;   mode-line-buffer-identification       ;Buffer name
;;   mode-line-position                    ;minor mode like column-number-mode, size etc
;;   (vc-mode vc-mode)
;;   ...
;;   ))

(provide 'sarcasm-mode-line)
