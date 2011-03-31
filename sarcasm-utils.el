;; Utility functions -- Guillaume Papin
;; usage:
;; (require 'sarcasm-utils)

(defun align-region-or-current ()
  "If a region is active align the region, otherwise align at
point."
  (interactive)
  (if mark-active
      (align (region-beginning) (region-end))
    (align-current))
  )

(defun c-man (NAME)
  "Find the C manual page corresponding to the function NAME.
Search for man(2) and man(3) by default."
  (if (file-exists-p (concat "/usr/share/man/man2/" NAME ".2.gz"))
      (man (concat NAME "(2)"))
    (if (file-exists-p (concat "/usr/share/man/man3/" NAME ".3.gz"))
	(man (concat NAME "(3)"))
      (man NAME)))
  )

(defun c-man-at-point ()
  "Find a C man page with the current word if present, otherwise
require input from user."
  (interactive)
  (when (not (setq cur-word (current-word)))
    (setq cur-word (read-from-minibuffer "Man Page: ")))
  (if (string= "" cur-word)
      (message "No man args given")
    (c-man cur-word))
  )

(provide 'sarcasm-utils)
