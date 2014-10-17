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


(add-to-list 'el-get-recipe-path (concat *sarcasm-directory*
                                         "sarcasm-recipes"))

(setq el-get-user-package-directory (concat *sarcasm-directory*
                                            "sarcasm-packages"))

(el-get 'sync)

;; "Addons" to the YASnippet config and auto-complete

;; Function found here: http://www.emacswiki.org/emacs/tagging.el
(defun convert-mode-name-to-hook (mode-name)
  "Converts a mode name into the symbol for its hook"
  (intern (concat (symbol-name mode-name) "-hook")))

;; Enable yasnippet mode and auto-complete on few programming modes
(defun sarcasm-enable-prog-mode ()
  ;; (yas/minor-mode-on)
  (goto-address-prog-mode 1)

  ;; This is certainly not the good place for that...but for the
  ;; moment it's ok
  (setq show-trailing-whitespace t))

(mapc (lambda (mode)
        (add-hook (convert-mode-name-to-hook mode) 'sarcasm-enable-prog-mode))
      '(c-mode c++-mode emacs-lisp-mode lisp-mode lua-mode js2-mode js-mode
               sh-mode perl-mode css-mode html-mode nxml-mode
               python-mode ruby-mode snippet-mode slime-mode
               slime-repl-mode))

(when (require 'cmake-mode nil t)
  (setq auto-mode-alist
        (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                  ("\\.cmake\\'" . cmake-mode))
                auto-mode-alist)))

(provide 'sarcasm-el-get)
