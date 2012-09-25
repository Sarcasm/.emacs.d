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

(when (file-exists-p "/usr/share/emacs/share-lisp/")
  (add-to-list 'load-path "/usr/share/emacs/share-lisp/")
  (load "ninja-mode"))

(let ((ledger-path (concat user-emacs-directory
			   (file-name-as-directory "ledger"))))
  (when (file-exists-p ledger-path)
    (add-to-list 'load-path ledger-path)
    (require 'ledger)
    (add-to-list 'auto-mode-alist '("\\.ledger\\'" . ledger-mode))))

;; Use `C-x 8 E' as a shortcut to get the euro sign (C-x 8 RET "EURO SIGN")
(require 'iso-transl)
(define-key 'iso-transl-ctl-x-8-map "E" [?€])

;; For libRocket configuration file (syntax based on HTML and CSS)
;; see: http://librocket.com

;; http://librocket.com/wiki/documentation/RML
(add-to-list 'auto-mode-alist '("\\.rml\\'" . html-mode))
;; http://librocket.com/wiki/documentation/RCSS
(add-to-list 'auto-mode-alist '("\\.rcss\\'" . css-mode))
