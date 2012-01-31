;; YASnippet stuff -- Guillaume Papin
;; usage:
;; (require 'sarcasm-yas)

(let ((yas-dir (concat user-emacs-directory
                       (file-name-as-directory "elisp")
                       "yasnippet")))
  (when (file-exists-p yas-dir)
    (add-to-list 'load-path yas-dir)))

(when (require 'yasnippet nil t)
  ;; from here: https://github.com/blastura/dot-emacs/blob/master/init.el
  ;; (add-hook 'yas/after-exit-snippet-hook
  ;;           '(lambda ()
  ;;              (indent-region yas/snippet-beg
  ;;                             yas/snippet-end)))

  ;; ;; After el-get/yasnippet, personal snippets takes priority
  ;; (setq yas/snippet-dirs (cons (concat el-get-dir "yasnippet/snippets")
  ;;                              '("~/.emacs.d/snippets")))

  ;; Map `yas/load-directory' to every element
  ;; (mapc 'yas/load-directory yas/snippet-dirs)

  ;; Fix the promp on X, the default was ugly.
  (require 'dropdown-list)
  (setq yas/prompt-functions '(yas/ido-prompt
                               yas/dropdown-prompt
                               yas/completing-prompt
                               yas/no-prompt)))

(provide 'sarcasm-yas)
