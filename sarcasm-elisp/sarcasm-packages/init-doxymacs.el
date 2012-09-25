(defun sarcasm-doxymacs-setup ()
  (setq doxymacs-command-character "\\")
  (doxymacs-mode 1)

  ;; (add-to-list 'doxymacs-doxygen-dirs
  ;;              '("/C\\+\\+/babel/"
  ;;                "~/projects/C++/babel/build/doc/babel.tag"
  ;;                "file:///mnt/media/projects/C++/babel/build/doc/html"))
  )

(add-hook 'c-mode-common-hook 'sarcasm-doxymacs-setup)

(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))

(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
