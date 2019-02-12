;; Thx: http://martinowen.net/blog/2010/02/tips-for-emacs-ibuffer.html
(defun sarcasm-ibuffer-other-window ()
                  "Open ibuffer in other window."
  (interactive)
  (ibuffer t))

(defun sarcasm-ibuffer-mode-init ()
  (ibuffer-auto-mode 1)                 ;auto update
  (ibuffer-switch-to-saved-filter-groups "default"))

(use-package ibuffer
  :bind ("C-x C-b" . sarcasm-ibuffer-other-window)
  :hook (ibuffer-mode . sarcasm-ibuffer-mode-init)
  :config
  (add-to-list 'ibuffer-formats '(mark
                                  modified
                                  " "
                                  (name 25 25 :left :elide)
                                  " "
                                  (size 9 -1 :right)
                                  " "
                                  (mode 16 16 :left :elide)
                                  " " filename-and-process))
  (setq ibuffer-saved-filter-groups
        '(("default"

           ("Interactive" (or (mode . lisp-interaction-mode)
                              (name . "\*Messages\*")
                              (name . "\*compilation\*")
                              (name . "\*Customize\*")
                              (name . "\*ag search\*")
                              (name . "\*grep\*")))

           ("Dired" (mode . dired-mode))

           ;; Need to be before "Programming" otherwise
           ;; `emacs-lisp-mode' will match.
           ("Emacs config" (filename . ".emacs.d/sarcasm-elisp"))

           ("Org-Mode" (mode . org-mode))

           ("Programming" (or (mode . c-mode)
                              (mode . c++-mode)
                              (mode . makefile-mode)
                              (mode . ruby-mode)
                              (mode . perl-mode)
                              (mode . python-mode)
                              (mode . js-mode)
                              (mode . js2-mode)
                              (mode . css-mode)
                              (mode . emacs-lisp-mode)))

           ("Mail/Gnus" (or (mode . gnus-group-mode)
                            (mode . gnus-server-mode)
                            (mode . gnus-summary-mode)
                            (mode . gnus-browse-mode)
                            (mode . gnus-article-mode)
                            (mode . message-mode)
                            (mode . mail-mode)))

           ("Magit" (name . "\*magit"))

           ("IRC" (or (mode . erc-mode)
                      (mode . rcirc-mode)))

           ("Help" (or (name . "\*Help\*")
                       (name . "\*Apropos\*")
                       (name . "\*info\*")))

           ("Man" (mode . Man-mode)))))
  )

(use-package ibuf-ext
  :defer t
  :config
  ;; defined in dired.el
  ;; (add-to-list 'ibuffer-never-show-predicates (regexp-opt sarcasm-ignored-files))
  ;; (setq ibuffer-never-show-predicates (append ibuffer-never-show-predicates
  ;;                                             sarcasm-ignored-files-re))

  ;; Gnus specific buffers
  (add-to-list 'ibuffer-never-show-predicates "\*nnimap ")
  (add-to-list 'ibuffer-never-show-predicates "\*imap log\*")
  (add-to-list 'ibuffer-never-show-predicates "\*gnus trace\*")
  )
