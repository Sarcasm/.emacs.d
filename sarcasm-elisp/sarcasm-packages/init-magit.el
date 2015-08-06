(setq magit-repo-dirs '("/mnt/media/projects/"))

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
