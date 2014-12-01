;; ;; Map `yas/load-directory' to every element
;; (mapc 'yas/load-directory yas/snippet-dirs)

;; ;; Fix the prompt on X, the default was ugly.
;; (when (require 'dropdown-list nil t)
;;   (setq yas/prompt-functions '(yas/ido-prompt
;;                                yas/dropdown-prompt
;;                                yas/completing-prompt
;;                                yas/no-prompt)))

(defconst sarcasm-snippet-dir (locate-user-emacs-file "snippets"))

;; ensure the snippets dir exists
(unless (file-exists-p sarcasm-snippet-dir)
  (make-directory sarcasm-snippet-dir))

;; Disable the load of the default snippets, I prefer to use mine or none of
;; them
;;
;; Even without snippets this is useful for package like irony-mode that uses
;; yasnippet as a dynamic snippet library.
(setq yas-snippet-dirs (list sarcasm-snippet-dir))

(yas-global-mode 1)

(defun sarcasm-disable-yas ()
  (yas-minor-mode -1))

;;buggy in `serial-term' at work
(add-hook 'term-mode-hook 'sarcasm-disable-yas)
