;;; init.el --- Emacs user configuration file  -*- lexical-binding: t; -*-

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(setq inhibit-startup-screen t)
(setq truncate-lines t)

(with-eval-after-load 'package
  (when (< emacs-major-version 28)
    (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/")))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (setq package-archive-priorities '(("gnu" . 20)
				     ("nongnu" . 10)
				     ("melpa" . 0))))

;; When using C-x C-e to edit the command line
(add-to-list 'auto-mode-alist '("^/tmp/zsh" . sh-mode))

(add-to-list 'auto-mode-alist '("\\.clang-\\(?:format\\|tidy\\)\\'" . yaml-mode))

(add-hook 'eglot-server-initialized-hook #'yas-minor-mode)

(defun sarcasm-eglot-yas ()
  (yas-minor-mode (eglot-managed-p)))
(add-hook 'eglot-managed-mode-hook #'sarcasm-eglot-yas)
(add-hook 'c++-mode-hook #'eglot-ensure)

(autoload 'sarcasm-clang-format-set-c-style "sarcasm-clang-format")
(defun sarcasm-set-c++-cc-style ()
  "Personalized cc-style for c++ mode."
  (c-set-offset 'innamespace 0)
  (sarcasm-clang-format-set-c-style))
(add-hook 'c++-mode-hook 'sarcasm-set-c++-cc-style)

(progn ;; git-commit
  (defun sarcasm-git-commit-setup ()
    """Enable git-commit mode even when `git-commit' hasn't been loaded yet.

This is useful when using git from the command line, e.g.:

    EDITOR='emacs -nw' git commit

At the time of this writing,
there are no autoloads providing the necessary features to do this cleanly,
see https://github.com/magit/magit/pull/4352.
"""
    ;; based on git-commit-filename-regexp
    (let ((git-msg-or-desc-re "/\\(\
\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|MERGEREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\
\\|\\(BRANCH\\|EDIT\\)_DESCRIPTION\\)\\'"))
      (when (and buffer-file-name
                 (string-match-p git-msg-or-desc-re buffer-file-name)
                 (not (featurep 'git-commit)))
        (require 'git-commit)
        (or git-commit-mode (git-commit-setup)))))

  (add-hook 'find-file-hook #'sarcasm-git-commit-setup)

  (with-eval-after-load 'git-commit
    ;; When redacting commit message in magit,
    ;; with the diff view in the other window,
    ;; use company-dabbrev[-code] to complete words of the other buffer
    ;; in the commit message buffer.
    ;; Stolen from https://github.com/company-mode/company-mode/issues/704#issuecomment-325783249
    (remove-hook 'find-file-hook #'sarcasm-git-commit-setup)

    (defun sarcasm-company-dabbrev-ignore-except-magit-diff (buffer)
      (let ((name (buffer-name)))
        (and (string-match-p "\\`[ *]" name)
             (not (string-match-p "\\*magit-diff:" name)))))

    (defun sarcasm-git-commit-setup-hook ()
      (setq-local company-backends '(company-capf company-dabbrev-code company-dabbrev))
      (setq-local company-dabbrev-code-modes '(text-mode magit-diff-mode))
      (setq-local company-dabbrev-code-other-buffers 'code)
      (setq-local company-dabbrev-downcase nil)
      (setq-local company-dabbrev-ignore-buffers
                  #'sarcasm-company-dabbrev-ignore-except-magit-diff)
      (setq-local company-dabbrev-ignore-case nil))

    (add-hook 'git-commit-setup-hook #'sarcasm-git-commit-setup-hook)))

;; git-revise
;; copy-pasted git-rebase-todo configuration at bottom of magit/git-rebase-mode.el
(let ((git-revise-filename-regexp "/git-revise-todo\\'"))
  (add-to-list 'auto-mode-alist
               (cons git-revise-filename-regexp 'git-rebase-mode))

  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude git-revise-filename-regexp))

  (with-eval-after-load 'with-editor
    (add-to-list 'with-editor-server-window-alist
                 (cons git-revise-filename-regexp 'switch-to-buffer))
    (add-to-list 'with-editor-file-name-history-exclude git-revise-filename-regexp)))

(progn ;; org-mode
  (define-key mode-specific-map "oa" #'org-agenda)
  (define-key mode-specific-map "ol" #'org-store-link)
  (define-key mode-specific-map "oc" #'org-capture)
  (define-key mode-specific-map "oo" #'org-open-at-point-global)
  (define-key mode-specific-map "oi" #'org-insert-link-global)

  (setq-default org-log-done t
                org-src-fontify-natively t ;display specific mode colors in src block
                org-insert-mode-line-in-empty-file t
                org-hide-emphasis-markers t
                org-startup-folded t
                org-capture-bookmark nil
                org-hide-leading-stars t)

  (with-eval-after-load 'org
    (setq org-default-notes-file (expand-file-name "inbox.org" org-directory))
    (setq org-adapt-indentation nil))

  ;; Make windmove work in Org-Mode:
  ;; https://orgmode.org/manual/Conflicts.html
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right))

(progn ;; compile
  (defun sarcasm-truncate-lines-on ()
    (toggle-truncate-lines 1))

  (add-hook 'compilation-mode-hook 'sarcasm-truncate-lines-on))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bookmark-save-flag 1)
 '(comment-style 'extra-line)
 '(company-backends '(company-capf))
 '(compilation-scroll-output 'first-error)
 '(completions-detailed t)
 '(dired-listing-switches "-alhv" nil nil "natural sorting helps sort files like fs.cpp, fs.h, fs.test.cpp together, and not fslite.h before fs.test.cpp")
 '(fill-column 80)
 '(global-hl-line-sticky-flag t)
 '(indent-tabs-mode nil)
 '(kill-whole-line t nil nil "C-k kills whole line and newline if at beginning of line")
 '(magit-diff-refine-hunk t)
 '(mouse-yank-at-point t)
 '(org-modules '(ol-docview ol-info org-mouse org-tempo))
 '(package-selected-packages
   '(clang-format yasnippet ledger-mode multiple-cursors ace-window buffer-move markdown-mode dockerfile-mode yaml-mode strace-mode company eglot vertico magit))
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(safe-local-variable-values '((eval c-set-offset 'innamespace 0)))
 '(save-abbrevs 'silently nil nil "don't want to answer yes everytime")
 '(scroll-preserve-screen-position t nil nil "restore cursor after PgUp/PgDown")
 '(sentence-end-double-space nil nil nil "sentences end with one space when M-q `fill-paragraph' is called")
 '(yank-pop-change-selection t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun sarcasm-cache (file)
  (let ((path (expand-file-name (convert-standard-filename file)
                                (expand-file-name (convert-standard-filename ".cache/")
                                                  user-emacs-directory))))
    (make-directory (file-name-directory path) t)
    path))

;; backup and autosave files
;; - http://www.emacswiki.org/emacs/BackupDirectory
;; - http://snarfed.org/gnu_emacs_backup_files
(setq backup-by-copying         t       ; don't clobber symlinks
      backup-directory-alist    (list (cons "." (sarcasm-cache "backup")))
      delete-old-versions       t
      kept-new-versions         6
      kept-old-versions         2
      version-control           t)      ; use versioned backups

(let ((autosave-dir (file-name-as-directory (sarcasm-cache "auto-save"))))
  (setq auto-save-list-file-prefix autosave-dir
        auto-save-file-name-transforms
        (list (list ".*" (concat autosave-dir "\\1") t)))
  ;; create the autosave dir if necessary, since emacs won't.
  (make-directory autosave-dir t))

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

(with-eval-after-load 'dired
  (require 'dired-x)
  (defvar sarcasm-ignored-files '("GPATH" "GRTAGS" "GTAGS"
                                  ".git" ".gitignore"
                                  ".hg" ".hgignore"
                                  "__pycache__"
                                  ;; core.clj was ignored in `dired-omit-files'
                                  ;; "core"
                                  "vgcore"
                                  ".newsrc-dribble")
    "A list of filename and directory to ignore (no directory
separator should be involved).

note: at this time this variable is used for making the
`dired-omit-files' and some `eproject' project type.")

  (defvar sarcasm-ignored-files-re '("cscope\\.\\w+"
                                     ;; coredump and valgrind coredump
                                     "\\(?:\\vg\\)?core\\.[[:digit:]]+")
    "See `sarcasm-ignored-files'.

There only difference is that each filename should be a regexp.")

  (setq dired-omit-files (concat (and dired-omit-files
                                      (concat dired-omit-files "\\|"))
                                 "^\\.\\|"
                                 (format "^%s\\|%s$"
                                         (regexp-opt sarcasm-ignored-files)
                                         (mapconcat 'identity
                                                    sarcasm-ignored-files-re
                                                    "\\|")))
        ;; The variable named FILE is the name of the file
        dired-guess-shell-alist-user
        (list '("\\.avi$" '("mplayer" "vlc"))
              ;; possibly more rules...
              '("\\.html$";; rule with condition test
                ;; Yes this is useless, just in order to remember the
                ;; ability to make conditional commands
                (let ((browser (getenv "BROWSER")))
                  (if (string= "index.html" file)
                      (concat browser " " file)
                    (concat browser " " file)))))))
(add-hook 'dired-mode-hook #'dired-omit-mode)
