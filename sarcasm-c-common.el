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

  ;; (c-toggle-auto-newline 1) ;not so handy when autopair is enable

  ;; It's better like that (School like tab alignement...)
  (setq align-indent-before-aligning    t
        align-to-tab-stop               t)

  ;; Adding a final newline when none are present. If I'm right the C
  ;; mode already do this automatically because it defined the
  ;; variable `mode-require-final-newline' to t.
  ;; (set (make-local-variable 'require-final-newline) t)

  (define-key c-mode-base-map (kbd "M-n") 'flymake-or-compile-next-error)
  (define-key c-mode-base-map (kbd "M-p") 'flymake-or-compile-prev-error)
  (define-key c-mode-base-map (kbd "C-c c") '(lambda ()
                                               (interactive)
                                               (save-buffer 0)
                                               (compile "make -k re")
                                               ))
  )

(add-hook 'c-mode-common-hook 'sarcasm-c-mode-common-hook)

(provide 'sarcasm-c-common)
