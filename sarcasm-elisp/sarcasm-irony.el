;; Irony-Mode stuff -- Guillaume Papin
;; usage:
;; (require 'sarcasm-irony)

(add-to-list 'load-path "/mnt/media/projects/Perso/irony-mode/elisp/")

(require 'irony)

;; The Clang installation missed the system include directory
;; "/usr/lib/clang/3.2/include/"
(when (file-exists-p "/usr/lib/clang/3.2/include/")
  (setq irony-libclang-additional-flags
        '("-isystem" "/usr/lib/clang/3.2/include/")))

(irony-enable 'ac)
;; (setq-default ac-sources nil)

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)

(provide 'sarcasm-irony)
