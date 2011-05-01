;; Restoring Emacs at startup -- Guillaume Papin
;; usage:
;; (require 'sarcasm-session)

;; Thx Emacs-Wiki
;; http://www.emacswiki.org/emacs/DeskTop

;; Automatically Saving the Desktop Periodically (part 1)
(setq *sarcasm-desktop-dir* (expand-file-name "~/.emacs.d/desktop"))
(setq desktop-dir *sarcasm-desktop-dir*)
(setq desktop-path (list *sarcasm-desktop-dir*))

(desktop-save-mode 1)
(setq desktop-restore-eager 10)     ;reload only 10 buffers at startup

;; Things to ignore
(setq desktop-buffers-not-to-save
      (concat "\\("
              "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
              "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
              "\\)$"))
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

;; Watch `desktop-globals-to-save' to save additionnal variables
;; e.g: (add-to-list 'desktop-globals-to-save 'file-name-history)

;; Automatically Saving the Desktop Periodically (part 2)
(setq *sarcasm-desktop-file* (concatenate 'string desktop-dir
                                      "/" desktop-base-file-name))
(setq *sarcasm-desktop-lock* (concatenate 'string desktop-dir
                                      "/" desktop-base-lock-name))
(defun desktop-in-use-p ()
  (and (file-exists-p *sarcasm-desktop-file*) (file-exists-p *sarcasm-desktop-lock*)))
(defun autosave-desktop ()
  (if (desktop-in-use-p) (desktop-save-in-desktop-dir)))
;; Can be switched off with (cancel-timer *sarcasm-desktop-saver-timer*)
(add-hook 'after-init-hook
          (lambda ()
            (setq *sarcasm-desktop-saver-timer*
                  (run-with-timer 5 300 'autosave-desktop))))

(provide 'sarcasm-session)
