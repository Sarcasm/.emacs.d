(when (equal system-type 'windows-nt)
  (let ((windows-git-path "c:/Program Files (x86)/Git/bin/"))
    (when (file-exists-p windows-git-path)
      (setenv "PATH" (concat windows-git-path ";" (getenv "PATH")))
      (setq exec-path (cons windows-git-path exec-path)))))

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
