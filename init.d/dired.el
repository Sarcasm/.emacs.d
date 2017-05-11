(defvar sarcasm-ignored-files '("GPATH" "GRTAGS" "GTAGS"
                                ".git" ".gitignore"
                                ".hg" ".hgignore"
                                "__pycache__"
                                ;; core.clj was ignored in `dired-omit-files'
                                ;; "core"
                                "vgcore"
                                ".newsrc-dribble")
  "A list of filename and directory to ignore (no directory
separator should be involved).

note: at this time this variable is used for making the
`dired-omit-files' and some `eproject' project type.")

(defvar sarcasm-ignored-files-re '("cscope\\.\\w+"
                                   ;; coredump and valgrind coredump
                                   "\\(?:\\vg\\)?core\\.[[:digit:]]+")
  "See `sarcasm-ignored-files'.

There only difference is that each filename should be a regexp.")

(use-package dired
  :defer t
  ;; Add the -h option (display size in the human readable form).
  :config (setq dired-listing-switches "-alh"))

(use-package dired-x
  :defer 0         ;lazily load so that C-x C-j and other features are available
  :bind (:map dired-mode-map
              ("M-o" . dired-omit-mode))
  :init
  (add-hook 'dired-mode-hook 'dired-omit-mode)
  :config
  (setq dired-omit-files (concat (and dired-omit-files
                                      (concat dired-omit-files "\\|"))
                                 "^\\.\\|"
                                 (format "^%s\\|%s$"
                                         (regexp-opt sarcasm-ignored-files)
                                         (mapconcat 'identity
                                                    sarcasm-ignored-files-re
                                                    "\\|")))
        ;; The variable named FILE is the name of the file
        dired-guess-shell-alist-user
        (list '("\\.avi$" '("mplayer" "vlc"))
              ;; possibly more rules...
              '("\\.html$";; rule with condition test
                ;; Yes this is useless, just in order to remember the
                ;; ability to make conditional commands
                (let ((browser (getenv "BROWSER")))
                  (if (string= "index.html" file)
                      (concat browser " " file)
                    (concat browser " " file)))))))
