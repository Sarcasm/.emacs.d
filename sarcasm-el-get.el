;; el-get packages and config -- Guillaume Papin
;; usage:
;; (require 'sarcasm-el-get)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/el-get/el-get/"))
(require 'el-get)

(setq el-get-sources
      '(el-get magit

               (:name switch-window
                      ;; re-define `C-x o' to `switch-window' because
                      ;; it doesn't work the first time...
                      after: (lambda ()
                               (global-set-key (kbd "C-x o") 'switch-window)
                               )
                      )

        (:name yasnippet
               :type svn
               :url "http://yasnippet.googlecode.com/svn/trunk/"
               ;; El-get default rule compile *.el, but with
               ;; yasnippet-debug.el it failed. In the Rakefile the
               ;; correct task seems to be 'rake compile'
               :build ("rake compile")
               :features yasnippet
               :post-init (lambda ()
                            (yas/initialize)
                            ;; (setq yas/snippet-dirs "~/.emacs.d/snippets")
                            ;; (yas/load-directory yas/snippet-dirs)

                            (setq yas/snippet-dirs (concat el-get-dir "yasnippet/snippets"))
                            ;; (add-to-list 'yas/snippet-dirs (concat el-get-dir "yasnippet/snippets"))
                            (yas/reload-all)
                            ;; (yas/load-directory yas/snippet-dirs)
                            ;; (yas/load-directory "~/.emacs.d/snippets")


                            ;; Fix the promp on X, the default was ugly.
                            (setq yas/prompt-functions '(yas/dropdown-prompt
                                                         yas/ido-prompt
                                                         yas/completing-prompt
                                                         yas/ido-prompt
                                                         yas/no-prompt))

                            ;; from here: https://github.com/blastura/dot-emacs/blob/master/init.el
                            ;; (add-hook 'yas/after-exit-snippet-hook
                            ;;           '(lambda ()
                            ;;              (indent-region yas/snippet-beg
                            ;;                             yas/snippet-end)))
                            ))

        (:name autocomplete
               :type git
               :url "http://github.com/m2ym/auto-complete.git"
               :load-path "."
               :post-init (lambda ()
                            (require 'auto-complete)
                            (add-to-list 'ac-dictionary-directories (expand-file-name "dict" pdir))
                            (require 'auto-complete-config)
                            (ac-config-default)
                            ;; Too many words in buffers...
                            (setq-default ac-sources (remq 'ac-source-words-in-same-mode-buffers ac-sources))
                            ))

        (:name auto-complete-extension
               :type emacswiki)

        (:name auto-complete-clang
               :type git
               :url "https://github.com/brianjcj/auto-complete-clang.git"
               :features auto-complete-clang
               :post-init (lambda ()
                            (define-key c++-mode-map (kbd "M-TAB") 'ac-complete-clang)
                            ))

        ))

;; Initialize el-get packages
(el-get)

(provide 'sarcasm-el-get)
