;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; This could be Windows-specific but since I never use VC I think it's better
;; to disable it altogether, I just use magit in practice.
;;
;; Disable VC hooks
;; http://stackoverflow.com/questions/6724471/git-slows-down-emacs-to-death-how-to-fix-this
(setq vc-handled-backends nil)

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

;; Remove VC handling when because it's annoying when using SSHFS and magit is
;; the only thing I really use for now.
(setq vc-handled-backends nil)
;; (remove-hook 'find-file-hooks 'vc-find-file-hook)
;; (delete 'Git vc-handled-backends)

;; (add-to-list 'load-path "~/.emacs.d/pieces-of-code/")
;; (require 'js-beautify)
;; (defun js-beautify-default-keybindings ()
;;   (local-set-key (kbd "C-S-f") 'js-beautify))
;; (add-hook 'js-mode-hook 'js-beautify-default-keybindings)
;; (add-hook 'js2-mode-hook 'js-beautify-default-keybindings)
;; (add-hook 'javascript-mode-hook 'js-beautify-default-keybindings)


(run-with-timer 2 nil (lambda ()
                        (set-cursor-color "gold")))

(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
