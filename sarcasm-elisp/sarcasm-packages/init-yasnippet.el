(setq yas/snippet-dirs (cons (concat el-get-dir "yasnippet/snippets")
                             '("~/.emacs.d/snippets")))

;; Map `yas/load-directory' to every element
(mapc 'yas/load-directory yas/snippet-dirs)

;; Fix the promp on X, the default was ugly.
(require 'dropdown-list)
(setq yas/prompt-functions '(yas/ido-prompt
                             yas/dropdown-prompt
                             yas/completing-prompt
                             yas/no-prompt))
