(require 'sarcasm)

(use-package yasnippet
  :ensure t
  :defer 2
  :config
  (let ((sarcasm-snippet-dir (locate-user-emacs-file "snippets")))
    ;; ensure the snippets directory exists
    (unless (file-exists-p sarcasm-snippet-dir)
      (make-directory sarcasm-snippet-dir))
    ;; Disable the load of the default snippets,
    ;; I prefer to use mine or none of them.
    ;; Even without snippets this is useful for package like `irony-mode'
    ;; that uses yasnippet as a dynamic snippet library.
    (setq yas-snippet-dirs (list sarcasm-snippet-dir)
          yas-verbosity 0)
    (sarcasm-deffered-global-mode #'yas-global-mode
                                  #'yas-minor-mode-on
                                  'yas-minor-mode)))
