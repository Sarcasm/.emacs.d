;; Flymake stuff -- Guillaume Papin
;; usage:
;; (require 'sarcasm-flymake)

(setq flymake-gui-warnings-enabled nil)

;; Add syntax checking for C++ headers files.
(push '("\\.hpp\\'" flymake-simple-make-init) flymake-allowed-file-name-masks)
(push '("\\.hh\\'" flymake-simple-make-init) flymake-allowed-file-name-masks)

;; ;; Flymake errors in the minibuffer
;; (defun flymake-show-help-in-minibuffer ()
;;   (when (get-char-property (point) 'flymake-overlay)
;;     (let ((help (get-char-property (point) 'help-echo)))
;;       (if help (message "%s" help))))
;;   )
;; ;; (add-hook 'post-command-hook 'flymake-show-help-in-minibuffer)

;; (defvar my-flymake-minor-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "\M-p" 'flymake-goto-prev-error)
;;     (define-key map "\M-n" 'flymake-goto-next-error)
;;     map)
;;   "Keymap for my flymake minor mode.")

;; (defun my-flymake-err-at (pos)
;;   (let ((overlays (overlays-at pos)))
;;     (remove nil
;;             (mapcar (lambda (overlay)
;;                       (and (overlay-get overlay 'flymake-overlay)
;;                            (overlay-get overlay 'help-echo)))
;;                     overlays))))

;; (defun my-flymake-err-echo ()
;;   (message "%s" (mapconcat 'identity (my-flymake-err-at (point)) "\n")))

;; (defadvice flymake-goto-next-error (after display-message activate compile)
;;   (my-flymake-err-echo))

;; (defadvice flymake-goto-prev-error (after display-message activate compile)
;;   (my-flymake-err-echo))

;; (define-minor-mode my-flymake-minor-mode
;;   "Simple minor mode which adds some key bindings for moving to the next and previous errors.

;; Key bindings:

;; \\{my-flymake-minor-mode-map}"
;;   nil
;;   nil
;;   :keymap my-flymake-minor-mode-map)

;; (defadvice flymake-mode (after sarcasm-flymake-mode activate
;; compile)
;;   "Add keymap to Flymake."
;;   (my-flymake-minor-mode)
;;   )

(provide 'sarcasm-flymake)
