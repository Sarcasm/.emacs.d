;; Load personnal config
(load-file (expand-file-name "~/.emacs.d/sarcasm-elisp/sarcasm.el"))

;; Basic Epitech files provide:
;; - headers
;; - few keys
(let ((std-file (expand-file-name "~/.emacs.d/elisp/std.el"))
      (std-comment-file (expand-file-name "~/.emacs.d/elisp/std_comment.el")))
  (when (and (file-exists-p std-comment-file)
             (file-exists-p std-file))
    (load-file std-comment-file)
    (load-file std-file)))
