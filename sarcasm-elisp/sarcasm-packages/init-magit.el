(setq magit-repo-dirs '("/mnt/media/projects/"))

;; Magit
(define-key mode-specific-map (kbd "x m") 'magit-status)
;; Imagine:
;; LOCAL <-> SERVER
;; push: local -> server
;; pull: local <- server
;; remove '-' and...HEY !!! It's '>' and '<' !
(add-hook 'magit-mode-hook '(lambda ()
                              (define-key magit-mode-map (kbd ">") 'magit-push)
                              (define-key magit-mode-map (kbd "<") 'magit-pull)
                              (define-key magit-mode-map (kbd "C-o")
                                (lambda ()
                                  (interactive)
                                  (magit-visit-item t)))))
