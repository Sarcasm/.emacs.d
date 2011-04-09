;; Flymake stuff -- Guillaume Papin
;; usage:
;; (require 'sarcasm-flymake)

;; Add syntax checking for C++ headers files.
(push '("\\.hpp\\'" flymake-simple-make-init) flymake-allowed-file-name-masks)
(push '("\\.hh\\'" flymake-simple-make-init) flymake-allowed-file-name-masks)

;; Flymake errors in the minibuffer
(defun flymake-show-help-in-minibuffer ()
  (when (get-char-property (point) 'flymake-overlay)
    (let ((help (get-char-property (point) 'help-echo)))
      (if help (message "%s" help))))
  )
(add-hook 'post-command-hook 'flymake-show-help-in-minibuffer)

(provide 'sarcasm-flymake)
