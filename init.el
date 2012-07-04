(when (equal system-type 'windows-nt)
  ;; Start emacs as a server because it seems to be impossible to have a daemonized emacs :(
  (server-start)

  ;; see http://emacs-fu.blogspot.com/2009/03/windows-and-daemons.html
  (defun my-done ()
    "Hide the Emacs server frame when finished."
    (interactive)
    (server-edit)
    (make-frame-invisible nil t))
  (global-set-key (kbd "C-x C-c") 'my-done)

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
