(autoload 'org-read-date "org")
(autoload 'ledger-context-at-point "ledger-mode")

(defun sarcasm-ledger-time-stamp ()
  "Insert timestamp at point a bit like `org-time-stamp'"
  (interactive)
  (require 'org)
  (let* ((date (parse-time-string (org-read-date)))
         (date-str (format "%04d/%02d/%02d" (nth 5 date) (nth 4 date)
                           (nth 3 date)))
         (context (car (ledger-context-at-point))))
    (cond
     ;; ((eq 'xact ) ;; replace existing date
     ;;  (beginning-of-line)
     ;;  (when (re-search-forward ledger-iso-date-regexp nil 'noerror)
     ;;  ledger-iso-date-regexp)
     (t ;; (memq context '(empty-line unknown))
      (insert date-str)))))

(defvar ledger-mode-map)
(defun sarcasm-ledger-init ()
  (interactive)
  ;; re-use org-time-stamp binding
  (define-key ledger-mode-map (kbd "C-c .") 'sarcasm-ledger-time-stamp))

(add-hook 'ledger-mode-hook 'sarcasm-ledger-init)

(add-to-list 'auto-mode-alist '("\\.ledger\\'" . ledger-mode))
