;; el-get packages and config -- Guillaume Papin
;; usage:
;; (require 'sarcasm-el-get)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/el-get/el-get/"))

(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
       (goto-char (point-max))
       (eval-print-last-sexp)))))


;; (add-to-list 'el-get-recipe-path (concat *sarcasm-directory*
;;                                          "sarcasm-recipes"))

(setq el-get-user-package-directory (concat *sarcasm-directory*
                                            "sarcasm-packages"))

(el-get 'sync)

;; "Addons" to the YASnippet config and auto-complete

;; Function found here: http://www.emacswiki.org/emacs/tagging.el
(defun convert-mode-name-to-hook (mode-name)
  "Converts a mode name into the symbol for its hook"
  (intern (concat (symbol-name mode-name) "-hook")))

;; Enable yasnippet mode and auto-complete on few programming modes
(defun sarcasm-enable-ac-and-yas ()
  "Enable `auto-complete' and `yasnippet'. Also add snippet names
in auto-complete sources."
  (yas/minor-mode-on) ;if not set before (auto-complete-mode 1), overlay persist after an expansion
  (auto-complete-mode 1)
  ;; Already present by default
  ;; (setq ac-sources (append ac-sources '(ac-source-yasnippet)))

  (goto-address-prog-mode 1)

  ;; This is certainly not the good place for that...but for the
  ;; moment it's ok
  (setq show-trailing-whitespace t))

(mapc (lambda (mode)
        (add-hook (convert-mode-name-to-hook mode) 'sarcasm-enable-ac-and-yas))
      '(c-mode c++-mode emacs-lisp-mode lisp-mode lua-mode
               sh-mode perl-mode css-mode html-mode nxml-mode
               python-mode ruby-mode snippet-mode slime-mode
               slime-repl-mode))

(provide 'sarcasm-el-get)
