;; Irony-Mode development stuff -- Guillaume Papin
;; usage:
;; (require 'sarcasm-irony)
;;

(defcustom sarcasm-irony-development-dir "~/projects/irony"
  "Directory that contains the irony-mode project and
  affiliated (e.g: company-irony).")

(when (file-exists-p sarcasm-irony-development-dir)
  ;; Irony mode
  (add-to-list 'load-path
               (expand-file-name "irony-mode" sarcasm-irony-development-dir))

  ;; ELPA generates these autoloads automatically for us. Try to mimick this so
  ;; that the configuration in sarcasm-packages/init-irony.el works seamlessly
  ;; between the developement package and the MELPA package.
  (autoload 'irony-mode "irony" nil t)
  (autoload 'irony-cdb-menu "irony-cdb" nil t)
  (autoload 'irony-cdb-autosetup-compile-options "irony-cdb" nil t)

  (load-file (concat *sarcasm-directory* "sarcasm-packages/init-irony.el"))

  ;; Flycheck Irony
  (add-to-list 'load-path (expand-file-name "flycheck-irony"
                                            sarcasm-irony-development-dir))
  (require 'flycheck-irony)
  (load-file (concat *sarcasm-directory* "sarcasm-packages/init-flycheck-irony.el"))

  ;; ;; Irony Eldoc
  ;; (add-to-list 'load-path (expand-file-name "irony-eldoc"
  ;;                                           sarcasm-irony-development-dir))
  ;; (require 'irony-eldoc)
  ;; (add-hook 'irony-mode-hook 'irony-eldoc)

  ;; Company Irony
  (add-to-list 'load-path
               (expand-file-name "company-irony" sarcasm-irony-development-dir))

  (autoload 'company-irony "company-irony" nil t)
  (autoload 'company-irony-setup-begin-commands "company-irony")

  (load-file (concat *sarcasm-directory* "sarcasm-packages/init-company-irony.el"))

  ;; Flycheck Irony
  (add-to-list 'load-path
               (expand-file-name "flycheck-irony" sarcasm-irony-development-dir))

  (require 'flycheck-irony)
  (load-file (concat *sarcasm-directory* "sarcasm-packages/init-flycheck-irony.el"))

  ;; Irony Eldoc
  (add-to-list 'load-path
               (expand-file-name "irony-eldoc" sarcasm-irony-development-dir))
  (require 'irony-eldoc)
  (add-hook 'irony-mode-hook 'irony-eldoc))

(provide 'sarcasm-irony)
