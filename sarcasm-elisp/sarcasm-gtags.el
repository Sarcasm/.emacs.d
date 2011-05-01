;; GTags stuff -- Guillaume Papin
;; usage:
;; (require 'sarcasm-gtags)

;; Gtags mode for easy browsing between symbols
;; Recent sources of gtags.el (allow gtags-*-other-window)
;; http://www.gnu.org/software/global/
;; http://cvs.savannah.gnu.org/viewvc/global/gtags.el?root=global&view=markup
;; (add-to-list 'load-path "~/.emacs.d/global/")
;; (autoload 'gtags-mode "gtags" "" t)
(load-file (expand-file-name "~/.emacs.d/sarcasm-elisp/global/gtags.el"))

;; Easy access to tags
(define-key gtags-mode-map (kbd "C-.") 'gtags-find-tag-from-here-other-window)
(define-key gtags-mode-map (kbd "M-.") 'gtags-find-tag-from-here)

;; Based on gtags-find-tag-from-here
(defun gtags-find-tag-from-here-other-window ()
  "Find tags at point and open in other window."
  (interactive)
  (let (tagname flag)
    (setq tagname (gtags-current-token))
    (when tagname
      ;; other-window ok...but I want to stay in my current buffer
      (save-selected-window
	(gtags-push-context)
	(gtags-goto-tag tagname "C" t))))
  )

;; Come from the CEDET package
(require 'pulse)

(defadvice gtags-goto-tag (after gtags-goto-tag-my-advice
activate compile)
  "After a jump to the tag definition, temporary highlight the
line under the cursor."
  (pulse-momentary-highlight-one-line (point)))

(defadvice gtags-select-it (after gtags-select-it-my-advice
activate compile)
  "After a jump to the tag definition found in Global select
buffer, temporary highlight the line under the cursor."
  (pulse-momentary-highlight-one-line (point)))

(provide 'sarcasm-gtags)
