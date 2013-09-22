;; Custom settings -- Guillaume Papin
;; usage:
;;
;;     (setq custom-file (concat *sarcasm-directory*
;;                               "sarcasm-custom.el"))
;;     (load custom-file)
;;
;; Note: This file shouldn't be tracked for changes, changes made to
;; safe-variables and co isn't interesting to be committed. To ignore local
;; changes to the file the following command can be entered:
;;
;;      git update-index --assume-unchanged /path/to/sarcasm-custom.el
;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-doc-comment-style (quote ((java-mode . javadoc) (pike-mode . autodoc))))
 '(garak-alert-methods (quote (:notify))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
