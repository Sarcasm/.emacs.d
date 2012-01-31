;; Load personnal config
(load (concat user-emacs-directory
              (file-name-as-directory "sarcasm-elisp")
              "sarcasm.el"))

;; Basic Epitech files provide:
;; - headers
;; - few keybindings (school headers mainly)
(let ((std-file (expand-file-name "~/.emacs.d/elisp/std.el"))
      (std-comment-file (expand-file-name "~/.emacs.d/elisp/std_comment.el")))
  (when (and (file-exists-p std-comment-file)
             (file-exists-p std-file))
    (load-file std-comment-file)
    (load-file std-file)))

;; (add-to-list 'load-path "/mnt/media/projects/CodeWorker/kooc/tools/")
;; (require 'codeworker)


(add-to-list 'load-path "~/.emacs.d/elisp/groovy-mode/")
;; (require 'grails-mode)
;; (require 'groovy-mode)
;; (require 'inf-groovy)
;; (require 'groovy-electric)

;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;;; make Groovy mode electric by default.
(add-hook 'groovy-mode-hook
          '(lambda ()
             (require 'groovy-electric)
             (groovy-electric-mode)))
