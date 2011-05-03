;; Emacs backup files handling -- Guillaume Papin
;; usage:
;; (require 'sarcasm-backup)

;; source: http://www.emacswiki.org/emacs/BackupDirectory

(setq backup-by-copying         t       ; don't clobber symlinks
      backup-directory-alist    '(("." . "~/.emacs.d/backup")) ; don't litter my fs tree
      delete-old-versions       t
      kept-new-versions         6
      kept-old-versions         2
      version-control           t)      ; use versioned backups

;; source: http://snarfed.org/gnu_emacs_backup_files
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

(provide 'sarcasm-backup)
