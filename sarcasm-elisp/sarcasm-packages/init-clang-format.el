(require 'rx)
(require 'clang-format)

(defun clang-format-default-keybindings ()
  (define-key c-mode-base-map (kbd "C-S-f") 'clang-format-region))

;; Note, if a .dir-locals.el set some variables, it won't work, since local
;; variables are set after mode-hooks, they will be overriden, I think it make
;; sense to be able to override with .dir-locals.el so I'm not trying to counter
;; this. In case it appears to be necessary to counter it, see
;; http://stackoverflow.com/a/5148435/951426
(defun sarcasm-clang-format-set-c-style ()
  (let ((orig-path (or buffer-file-name default-directory))
        vars)
    (when (and orig-path
               (string= clang-format-style "file")
               (locate-dominating-file orig-path ".clang-format"))
      (with-temp-buffer
        (call-process clang-format-executable nil t nil
                      "-style" clang-format-style
                      "-dump-config")
        ;; TODO: narrow region to Langage Cpp?
        (goto-char (point-min))
        (when (re-search-forward (rx bol "IndentWidth"
                                     (zero-or-more blank)
                                     ":"
                                     (zero-or-more blank)
                                     (group (one-or-more digit)))
                                 nil t)
          (setq vars (cons (cons 'c-basic-offset
                                 (string-to-number (match-string 1)))
                           vars)))))
    (dolist (var vars)
      (set (car var) (cdr var)))))

(add-hook 'c++-mode-hook 'clang-format-default-keybindings)
(add-hook 'c-mode-hook 'clang-format-default-keybindings)

(add-hook 'c++-mode-hook 'sarcasm-clang-format-set-c-style)
