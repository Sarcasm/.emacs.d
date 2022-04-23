;;; init.el --- Emacs user configuration file  -*- lexical-binding: t; -*-

(setq inhibit-startup-screen t)
(setq truncate-lines t)

;; When using C-x C-e to edit the command line
(add-to-list 'auto-mode-alist '("^/tmp/zsh" . sh-mode))

(add-to-list 'auto-mode-alist '("\\.clang-\\(?:format\\|tidy\\)\\'" . yaml-mode))

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
  (add-hook 'org-shiftright-final-hook 'windmove-right)

  (with-eval-after-load 'org-agenda
    (setq org-agenda-files (list org-directory
                                 (expand-file-name "media" org-directory)))
    (setq org-agenda-skip-scheduled-if-done t)
    (add-to-list 'org-agenda-custom-commands
                 '("c" "Calendar" agenda ""
                   ((org-agenda-files (list "~/org/ink.org" "~/org/substance.org" "~/org/siggraph2020.org"))
                    (org-agenda-start-day "-7d")
                    (org-agenda-span 16)
                    (org-agenda-start-with-log-mode t)
                    (org-agenda-archives-mode t)
                    (org-agenda-include-inactive-timestamps 't)
                    (org-agenda-time-grid nil))))))

(progn ;; compile
  (defun sarcasm-truncate-lines-on ()
    (toggle-truncate-lines 1))

  (add-hook 'compilation-mode-hook 'sarcasm-truncate-lines-on))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-backends '(company-capf))
 '(compilation-scroll-output 'first-error)
 '(completions-detailed t)
 '(dired-listing-switches "-alhv" nil nil "natural sorting helps sort files like fs.cpp, fs.h, fs.test.cpp together, and not fslite.h before fs.test.cpp")
 '(indent-tabs-mode nil)
 '(kill-whole-line t nil nil "C-k kills whole line and newline if at beginning of line")
 '(magit-diff-refine-hunk t)
 '(org-modules '(ol-docview ol-info org-mouse org-tempo))
 '(package-selected-packages
   '(beancount ledger-mode multiple-cursors ace-window buffer-move markdown-mode dockerfile-mode yaml-mode strace-mode company eglot vertico magit)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
