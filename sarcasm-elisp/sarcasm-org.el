;; Org-Mode settings -- Guillaume Papin
;; usage:
;; (require 'sarcasm-org)

(add-to-list 'auto-mode-alist '("TODO" . org-mode))

(setq org-log-done t
      org-src-fontify-natively t ;display specific mode colors in src block
      org-agenda-files '("~/Org")
      org-archive-location "archives/%s_archive::"
      org-insert-mode-line-in-empty-file t
      org-src-window-setup 'other-window
      org-startup-with-inline-images t
      org-hide-emphasis-markers t
      org-hide-leading-stars t)

;; Org-Mode global keybindings
(define-key mode-specific-map (kbd "o a") 'org-agenda)
(define-key mode-specific-map (kbd "o l") 'org-store-link)
(define-key mode-specific-map (kbd "o o") 'org-open-at-point-global)
(define-key mode-specific-map (kbd "o i") 'org-insert-link-global)

(add-hook 'org-mode-hook
	  '(lambda ()
             (auto-fill-mode 1)
             (setq show-trailing-whitespace t)
             (define-key org-mode-map [f7] 'org-flyspell-mode-and-dictionary)

             (setq org-agenda-start-with-follow-mode t)
             ;; Like in *grep* buffers
             (define-key org-agenda-mode-map (kbd "C-c C-f") 'org-agenda-follow-mode)

             ;; Babel handle 'C' not 'c'...ok, in fact babel doesn't
             ;; know how to execute 'C' use 'c++' instead.
             (add-to-list 'org-src-lang-modes '("C" . c))
             (org-save-place-fix)))

;; Make windmove work in Org-Mode:
(add-hook 'org-shiftup-final-hook    'windmove-up)
(add-hook 'org-shiftleft-final-hook  'windmove-left)
(add-hook 'org-shiftdown-final-hook  'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;; Add hook for Flyspell dictionary on Org-Mode files
(defun org-flyspell-mode-and-dictionary()
  "Enable Flyspell mode and if the 'LANGUAGE: [lang]' header is set,
change the dictionnary to the corresponding language."
  (interactive)
  (if (not flyspell-mode)
      (progn
	(flyspell-mode)
	(let ((current-pos (point)))
	  (goto-char (point-min))
	  (when (re-search-forward "#\\+LANGUAGE:\s+\\(\\w+\\)" nil t 1)
	    (setq lang (match-string 1))
	    (ispell-change-dictionary lang))
	  (goto-char current-pos)
	  ))
    (flyspell-mode -1)))

(defun org-save-place-fix ()
  "When save place restore the cursor in a folded headline it's a
little buggy."
  (when (outline-invisible-p)
    (save-excursion
      (outline-previous-visible-heading 1)
      (org-show-subtree))))

		      ;; === Org-Babel stuff ===

;; Babel Mode for Org-Mode (enable code interpretation during export)
(when (require 'ob nil t)
  ;; ditaa path
  (setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")

  ;; Active Babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C      . t)
     (ditaa  . t)
     (sh     . t)
     (R      . t)
     (org    . t)
     (latex  . t)
     (python . t)
     (sql    . t)
     (dot    . t)))

  ;; It's really annoying to enter 'yes' every time I export a org-file
  ;; with ditaa diagrams. It's dangerous on Shell script for example,
  ;; should be used with caution.
  (setq org-confirm-babel-evaluate nil))

(provide 'sarcasm-org)
