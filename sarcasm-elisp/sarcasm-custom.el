;; Custom settings -- Guillaume Papin
;; usage:
;;
;;     (setq custom-file (concat *sarcasm-directory*
;;                               "sarcasm-custom.el"))
;;     (load custom-file)
;;
;; note: the file in is initial state is good, changes made by the
;; time for safe variable values for example are not interesting to
;; commit, so to ignore local changes to the file the following
;; command need to be entered:
;;
;;      git update-index --assume-unchanged /path/to/sarcasm-custom.el
;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["black" "firebrick" "lime green" "yellow2" "DodgerBlue2" "deep pink" "cornflower blue" "white"])
 '(blink-cursor-mode nil)
 '(c-doc-comment-style (quote ((java-mode . javadoc) (pike-mode . autodoc))))
 '(cc-other-file-alist (quote (("\\.hh\\'" (".cpp")) ("\\.cpp\\'" (".hh" ".hpp" ".h")) ("\\.cc\\'" (".hh" ".h")) ("\\.hh\\'" (".cc" ".C")) ("\\.c\\'" (".h")) ("\\.h\\'" (".c" ".cc" ".C" ".CC" ".cxx" ".cpp")) ("\\.C\\'" (".H" ".hh" ".h")) ("\\.H\\'" (".C" ".CC")) ("\\.CC\\'" (".HH" ".H" ".hh" ".h")) ("\\.HH\\'" (".CC")) ("\\.c\\+\\+\\'" (".h++" ".hh" ".h")) ("\\.h\\+\\+\\'" (".c++")) ("\\.cpp\\'" (".hpp" ".hh" ".h")) ("\\.hpp\\'" (".cpp")) ("\\.cxx\\'" (".hxx" ".hh" ".h")) ("\\.hxx\\'" (".cxx")))))
 '(column-number-mode t)
 '(custom-safe-themes (quote ("17bbdc3c63e63955c5b3e5cacbe70fede6e3f4b4885d0e213f371b6761f84c9b" "9ef30f9b4b4ca2daddfeafb3d921742444beef0d" "f1ff3c9a856c9d66780893e78a5de908cd5b958b" default)))
 '(garak-alert-methods (quote (:notify)))
 '(safe-local-variable-values (quote ((c-default-style . "linux"))))
 '(send-mail-function (quote smtpmail-send-it))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "grey10" :foreground "white smoke" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))
