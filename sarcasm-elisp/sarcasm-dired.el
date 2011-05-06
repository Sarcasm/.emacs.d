;; Dired stuff -- Guillaume Papin
;; usage:
;; (require 'sarcasm-dired)

;; Load Dired Extra
(require 'dired-x)

;; Always enable `dired-omit-mode'
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))

;; List of files to omit (can be a regexp, will be surrounded by ^ and
;; $). `dired-omit-localp' should be set to NO-DIR
(setq dired-omit-ignored-files '("GPATH" "GRTAGS" "GTAGS"
                                 "\\.git" "\\.gitignore"
                                 "cscope\\.\\w+"
                                 ))

(add-to-list 'dired-omit-extensions ".a")

;; Construct `dired-omit-files' with `dired-omit-ignored-files'
(setq dired-omit-files (concat dired-omit-files "\\|"
                               (mapconcat (function (lambda (file)
                                                      (concat "^" file "$")))
                                          dired-omit-ignored-files "\\|")))

;; The variable named FILE is the name of the file
(setq dired-guess-shell-alist-user
      (list
       (list "\\.avi$" "mplayer") ;fixed rule
       ;; possibly more rules...
       (list "\\.html$";; rule with condition test
             ;; Yes this is useless, just in order to remember the
             ;; abaility to make conditional commands
             '(if (string= "index.html" file)
                  (concat browse-url-generic-program " " file)
                (concat browse-url-generic-program " " file)))))

(provide 'sarcasm-dired)
