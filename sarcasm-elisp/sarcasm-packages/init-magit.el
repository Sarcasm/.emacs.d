;; Magit
(define-key mode-specific-map (kbd "x m") 'magit-status)

(add-hook 'magit-mode-hook 'sarcasm-magit-hook)

(defun sarcasm-magit-visit-item-other-window ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (if (fboundp 'magit-diff-visit-file)
        (call-interactively 'magit-diff-visit-file)
      (call-interactively 'magit-visit-item))))

;; Imagine:
;; LOCAL <-> SERVER
;; push: local -> server
;; pull: local <- server
;; remove '-' and...HEY !!! It's '>' and '<' !
(defun sarcasm-magit-hook ()
  (hl-line-mode -1)
  (define-key magit-mode-map (kbd ">") 'magit-push)
  (define-key magit-mode-map (kbd "<") 'magit-pull)
  (define-key magit-mode-map (kbd "C-o")
    'sarcasm-magit-visit-item-other-window))

(setq magit-last-seen-setup-instructions "1.4.0")

(defun repo-status ()
  (interactive)
  (let ((repodir (locate-dominating-file default-directory ".repo"))
        gitdirs)
    (unless repodir
      (error "Not in a repo workspace"))
    (setq gitdirs (process-lines "repo" "list" "-fp"))
    (let ((magit-repository-directories (mapcar (lambda (path)
                                                  (cons path 0))
                                                gitdirs)))
      (call-interactively 'magit-list-repositories))))

(define-key mode-specific-map (kbd "x r") 'repo-status)
