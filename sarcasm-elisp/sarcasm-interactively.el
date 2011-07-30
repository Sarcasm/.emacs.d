;; Minibuffer completion and other interactive commands -- Guillaume Papin
;; usage:
;; (require 'sarcasm-interactively)

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

(add-to-list 'ido-ignore-files "\\.gitignore")
(add-to-list 'ido-ignore-files "cscope\\.\\w+")
(add-to-list 'ido-ignore-files "GPATH\\|GRTAGS\\|GTAGS")

;; ibuffer settings
;; Thx: http://martinowen.net/blog/2010/02/tips-for-emacs-ibuffer.html
(global-set-key (kbd "C-x C-b")
                (lambda ()
                  "Open ibuffer in other window."
                  (interactive)
                  (ibuffer t)))

(setq ibuffer-saved-filter-groups
      '(("default"

         ("Interactive" (or (name . "\*scratch\*")
                            (name . "\*compilation\*")
                            (name . "\*grep\*")))

         ("Org-Mode" (mode . org-mode))

         ("Programming" (or (mode . c-mode)
                            (mode . c++-mode)
                            (mode . makefile-mode)
                            (mode . ruby-mode)
                            (mode . perl-mode)
                            (mode . python-mode)
                            (mode . emacs-lisp-mode)))

         ("Dired" (mode . dired-mode))

         ("Emacs config" (or (filename . ".emacs.d/sarcasm-elisp")
                             (filename . "emacs-config")))

         ("Magit" (name . "\*magit"))

         ("Mail" (or (mode . message-mode)
                     (mode . mail-mode)))

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

(provide 'sarcasm-interactively)
