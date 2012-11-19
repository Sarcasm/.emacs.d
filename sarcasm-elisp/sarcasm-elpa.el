;; ELPA (Emacs Lisp Package Archive) stuff -- Guillaume Papin
;; usage:
;; (require 'sarcasm-elpa)

;; This was installed by package-install.el.
;; This provides support for the package system and
;; interfacing with ELPA, the package archive.
;; Move this code earlier if you want to reference
;; packages in your .emacs.
;; (when (load (expand-file-name "~/.emacs.d/elpa/package.el"))
;;   (package-initialize))

(setq package-archives '(("ELPA"      . "http://tromey.com/elpa/")
			 ("gnu"	      . "http://elpa.gnu.org/packages/")
			 ("Marmalade" . "http://marmalade-repo.org/packages/")
                         ("Org-Mode" . "http://orgmode.org/elpa/")
                         ))

(provide 'sarcasm-elpa)
