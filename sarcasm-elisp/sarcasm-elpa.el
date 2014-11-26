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
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(setq package-enable-at-startup nil) ;see Info node `(emacs) Package Installation'
(package-initialize)

(dolist (package package-activated-list)
  (let ((init-package-file (format "%s/init-%s.el"
                                   (concat *sarcasm-directory* "sarcasm-packages")
                                   package)))
    (when (file-exists-p init-package-file)
      (load-file init-package-file))))

(provide 'sarcasm-elpa)
