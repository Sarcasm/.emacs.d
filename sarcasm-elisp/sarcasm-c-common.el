;; Settings common to C/C++ mode -- Guillaume Papin
;; usage:
;; (require 'sarcasm-c-common)

;; Customizations for all modes in CC Mode.
(defun sarcasm-c-mode-common-hook ()
  "Hooks run for C/C++ and friends mode."

  ;; GTags Mode
  (gtags-mode 1)

  ;; Highlights suspicious C and C++ constructions
  (global-cwarn-mode 1)

  ;; (auto-fill-mode 1)

  ;; (c-toggle-auto-newline 1) ;not so handy when autopair is enable

  ;; It's better like that (School like tab alignement...)
  (setq align-indent-before-aligning    t
        align-to-tab-stop               t)

  ;; Adding a final newline when none are present. If I'm right the C
  ;; mode already do this automatically because it defined the
  ;; variable `mode-require-final-newline' to t.
  ;; (set (make-local-variable 'require-final-newline) t)

  (define-key c-mode-base-map (kbd "C-c m") 'c-man-at-point)
  (define-key c-mode-base-map (kbd "M-n") 'flymake-or-compile-next-error)
  (define-key c-mode-base-map (kbd "M-p") 'flymake-or-compile-prev-error)
  (define-key c-mode-base-map (kbd "C-c c") '(lambda ()
                                               (interactive)
                                               (save-buffer 0)
                                               (compile "make -k re")
                                               ))

  (define-key c-mode-base-map (kbd "C-c r") 'semantic-symref-rename-local-variable)
  (define-key c-mode-base-map (kbd "C-c j") 'semantic-complete-jump-local)

  (font-lock-add-keywords nil
                          '(("\\<\\(FIXME\\):" 1 font-lock-warning-face t)))
  (font-lock-add-keywords nil
                          '(("\\<\\(TODO\\):" 1 font-lock-keyword-face t)))


  ;; Note (from find-file.el comments):
  ;; SEARCHING is carried out in a set of directories specified by the
  ;; `ff-search-directories' variable:
  ;;
  ;;     ("." "../../src" "../include/*" "/usr/local/*/src/*" "$PROJECT/src")
  (setq ff-always-in-other-window t)
  (define-key c++-mode-map (kbd "C-c t") 'ff-find-other-file))

(add-hook 'c-mode-common-hook 'sarcasm-c-mode-common-hook)

(provide 'sarcasm-c-common)
