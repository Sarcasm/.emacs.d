(setq magit-repo-dirs '("/mnt/media/projects/"))

;; Magit
(define-key mode-specific-map (kbd "x m") 'magit-status)
;; Imagine:
;; LOCAL <-> SERVER
;; push: local -> server
;; pull: local <- server
;; remove '-' and...HEY !!! It's '>' and '<' !
(add-hook 'magit-mode-hook 'sarcasm-magit-setup-keys)

(defun sarcasm-magit-visit-item-other-window ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'magit-visit-item)))

(defun sarcasm-magit-setup-keys ()
  (define-key magit-mode-map (kbd ">") 'magit-push)
  (define-key magit-mode-map (kbd "<") 'magit-pull)
  (define-key magit-mode-map (kbd "C-o")
    'sarcasm-magit-visit-item-other-window))

(setq magit-last-seen-setup-instructions "1.4.0")
