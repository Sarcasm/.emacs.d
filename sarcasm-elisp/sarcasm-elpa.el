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

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(provide 'sarcasm-elpa)
