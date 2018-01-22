(when load-file-name
  (setq custom-file load-file-name))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(bookmark-save-flag 1)
 '(column-number-mode t)
 '(comment-style (quote extra-line))
 '(fill-column 80)
 '(frame-resize-pixelwise t)
 '(global-hl-line-sticky-flag t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(kill-whole-line t nil nil "C-k kills whole line and newline if at beginning of line")
 '(menu-bar-mode nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1))) nil nil "Smooth mouse scrolling, one line at a time")
 '(mouse-yank-at-point t)
 '(package-selected-packages
   (quote
    (anaconda-mode py-yapf yasnippet ace-window company-c-headers flycheck cmake-mode buffer-move ag qml-mode dockerfile-mode ledger-mode magit use-package)))
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(save-abbrevs (quote silently) nil nil "don't want to answer yes everytime")
 '(scroll-bar-mode nil)
 '(scroll-preserve-screen-position t nil nil "restore cursor after PgUp/PgDown")
 '(sentence-end-double-space nil nil nil "sentences end with one space when M-q `fill-paragraph' is called")
 '(tool-bar-mode nil)
 '(yank-pop-change-selection t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; backup and autosave files
;; - http://www.emacswiki.org/emacs/BackupDirectory
;; - http://snarfed.org/gnu_emacs_backup_files
(setq backup-by-copying         t       ; don't clobber symlinks
      backup-directory-alist    (list (cons "." (cache "backup")))
      delete-old-versions       t
      kept-new-versions         6
      kept-old-versions         2
      version-control           t)      ; use versioned backups

(let ((autosave-dir (file-name-as-directory (cache "auto-save"))))
  (setq auto-save-list-file-prefix autosave-dir
        auto-save-file-name-transforms
        (list (list ".*" (concat autosave-dir "\\1") t)))
  ;; create the autosave dir if necessary, since emacs won't.
  (make-directory autosave-dir t))

(setq-default abbrev-file-name (cache "abbrev_defs")
              bookmark-default-file (cache "bookmarks")
              savehist-file (cache "history")
              tramp-persistency-file-name (cache "tramp"))

;; Enable some disabled commands
(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'org-narrow-to-subtree 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'widen 'disabled nil)
