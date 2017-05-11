;;; sarcasm-ledger.el --- Ledger helpers

(autoload 'org-read-date "org")
(autoload 'ledger-context-at-point "ledger-mode")

;;;###autoload
(defun sarcasm-ledger-time-stamp ()
  "Insert timestamp at point a bit like `org-time-stamp'."
  (interactive)
  (let* ((date (parse-time-string (org-read-date)))
         (date-str (format "%04d/%02d/%02d" (nth 5 date) (nth 4 date)
                           (nth 3 date)))
         (context (car (ledger-context-at-point))))
    (insert date-str)))

(provide 'sarcasm-ledger)
