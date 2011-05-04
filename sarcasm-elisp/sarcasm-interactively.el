;; Minibuffer completion and other interactive commands -- Guillaume Papin
;; usage:
;; (require 'sarcasm-interactively)

;; (iswitchb-mode 1)

;; Iswitchb with Arrow-keys
;; (defun iswitchb-local-keys ()
;;   (mapc (lambda (K)
;; 	  (let* ((key (car K)) (fun (cdr K)))
;; 	    (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
;; 	'(("<right>" . iswitchb-next-match)
;; 	  ("<left>"  . iswitchb-prev-match)
;; 	  ;; ("<up>"	. ignore	     )
;; 	  ;; ("<down>"	. ignore	     )
;; 	  )))
;; (add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

;; ;; Different M-x completion
;; (icomplete-mode 1)
;; (setq icomplete-prospects-height 1) ;; don't spam minibuffer
;; ;; (require 'icomplete+) ;; autoload with ELPA I think

(ido-mode 1)

(add-to-list 'ido-ignore-files "\\.gitignore")
(add-to-list 'ido-ignore-files "cscope\\.\\w+")
(add-to-list 'ido-ignore-files "GPATH\\|GRTAGS\\|GTAGS")

(provide 'sarcasm-interactively)
