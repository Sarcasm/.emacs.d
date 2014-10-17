;; Minibuffer completion and other interactive commands -- Guillaume Papin
;; usage:
;; (require 'sarcasm-interactively)

(require 'ibuffer)

;; (iswitchb-mode 1)

;; Iswitchb with Arrow-keys
;; (defun iswitchb-local-keys ()
;;   (mapc (lambda (K)
;;        (let* ((key (car K)) (fun (cdr K)))
;;          (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
;;      '(("<right>" . iswitchb-next-match)
;;        ("<left>"  . iswitchb-prev-match)
;;        ;; ("<up>"	. ignore             )
;;        ;; ("<down>"	. ignore             )
;;        )))
;; (add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

;; ;; Different M-x completion
;; (icomplete-mode 1)
;; (setq icomplete-prospects-height 1) ;; don't spam minibuffer
;; ;; (require 'icomplete+) ;; autoload with ELPA I think


;; Dot directory first for `ido-find-file', enter to go in dired mode
(setq ido-use-filename-at-point nil)
;; (setq ido-show-dot-for-dired t)
(setq ido-enable-flex-matching t)
;; Allow the same buffer to be open in different frames
(setq ido-default-buffer-method 'selected-window)

(ido-mode 1)
(ido-everywhere 1)

(add-to-list 'ido-ignore-files (regexp-opt sarcasm-ignored-files))
(setq ido-ignore-files (append ido-ignore-files
                               sarcasm-ignored-files-re))

;; From EmacsWiki: http://www.emacswiki.org/emacs/InteractivelyDoThings#toc10
(require 'bookmark)
(defun ido-bookmark-jump (bname)
  "*Switch to bookmark interactively using `ido'."
  (interactive (list (ido-completing-read "Bookmark: " (bookmark-all-names) nil t)))
  (bookmark-jump bname))
(global-set-key (kbd "C-x r b") 'ido-bookmark-jump)

;; ibuffer settings
;; Thx: http://martinowen.net/blog/2010/02/tips-for-emacs-ibuffer.html
(global-set-key (kbd "C-x C-b")
                (lambda ()
                  "Open ibuffer in other window."
                  (interactive)
                  (ibuffer t)))

(require 'ibuffer)

;; Enlarge the default limit for buffer name
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

(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1)      ;auto update
             (ibuffer-switch-to-saved-filter-groups "default")))

(require 'ibuf-ext)
(add-to-list 'ibuffer-never-show-predicates (regexp-opt sarcasm-ignored-files))
(setq ibuffer-never-show-predicates (append ibuffer-never-show-predicates
                                            sarcasm-ignored-files-re))

;; Gnus specific buffers
(add-to-list 'ibuffer-never-show-predicates "\*nnimap ")
(add-to-list 'ibuffer-never-show-predicates "\*imap log\*")
(add-to-list 'ibuffer-never-show-predicates "\*gnus trace\*")

(provide 'sarcasm-interactively)
