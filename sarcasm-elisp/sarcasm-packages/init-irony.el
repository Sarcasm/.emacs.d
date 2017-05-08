(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)

(defun sarcasm-irony-mode-hook ()
  (define-key irony-mode-map (kbd "C-c C-b") 'irony-cdb-menu)
  (define-key irony-mode-map (kbd "C-c =") 'irony-get-type))

(add-hook 'irony-mode-hook 'sarcasm-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(defun sarcasm-irony-cdb-not-found (command &rest args)
  (when (eq command 'get-compile-options)
    (message "Irony: compile options not found!")
    nil))


;; Windows performance tweaks
;;
(when (boundp 'w32-pipe-read-delay)
  (setq w32-pipe-read-delay 0))
;; Set the buffer size to 64K on Windows (from the original 4K)
(when (boundp 'w32-pipe-buffer-size)
  (setq irony-server-w32-pipe-buffer-size (* 64 1024)))
