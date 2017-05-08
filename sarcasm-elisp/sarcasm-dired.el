;; Dired stuff -- Guillaume Papin
;; usage:
;; (require 'sarcasm-dired)

;; Load Dired Extra
(require 'dired-x)

(setq dired-recursive-deletes 'top)

;; Add the -h option (display size in the human readable form).
(setq dired-listing-switches "-alh")

(defvar-local sarcasm-no-dired-omit nil)
(put 'sarcasm-no-dired-omit 'safe-local-variable 'booleanp)
(defun sarcasm-dired-mode-hook ()
  (unless sarcasm-no-dired-omit
    (dired-omit-mode 1))
  (define-key dired-mode-map "\M-o" 'dired-omit-mode))

;; Always enable `dired-omit-mode'
(add-hook 'dired-mode-hook 'sarcasm-dired-mode-hook)

(add-to-list 'dired-omit-extensions ".a")

(setq dired-omit-localp 'no-dir         ;see doc
      dired-omit-files (concat (and dired-omit-files
                                    (concat dired-omit-files "\\|"))
                               "^\\.\\|"
                               (format "^%s\\|%s$"
                                       (regexp-opt sarcasm-ignored-files)
                                       (mapconcat 'identity
                                                  sarcasm-ignored-files-re
                                                  "\\|"))))

;; The variable named FILE is the name of the file
(setq dired-guess-shell-alist-user
      (list
       (list "\\.avi$" "mplayer") ;fixed rule
       ;; possibly more rules...
       (list "\\.html$";; rule with condition test
             ;; Yes this is useless, just in order to remember the
             ;; ability to make conditional commands
             '(if (string= "index.html" file)
                  (concat browse-url-generic-program " " file)
                (concat browse-url-generic-program " " file)))))

(provide 'sarcasm-dired)
