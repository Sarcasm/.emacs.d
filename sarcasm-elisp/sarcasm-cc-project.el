;; C and C++ eproject settings -- Guillaume Papin
;; usage:
;; (require 'sarcasm-cc-project)

;; When the file ".eproject" is found it's considered to be the root
;; of the project.
;; The file ".eproject" is also the configuration file
;;
;; Note for metadata/attributes:
;; - function value get the project root as argument
;; - function value are computed once, if the function need to be
;;   called each time wrap with another lambda
;;   .i.e:
;;       (lambda (root) (lambda (args ...) ...)

(define-project-type sarcasm-cc (generic) (look-for ".eproject")
  ;; look-for can be replaced by any list expression, example:
  ;; (or (look-for ".eproject") (look-for "Makefile") (look-for "SConstruct"))
  :relevant-files (".*")

  ;;
  ;; `completion-ignored-extensions' is added automatically so no need
  ;; to duplicate this one.
  ;;
  :irrelevant-files sarcasm-project-irrelevant-files

  ;; example - src/File.cpp -> src::File.cpp
  ;; :file-name-map (lambda (root)
  ;;                  (lambda (root filename)
  ;;                    (replace-regexp-in-string "/" "::" filename)))
  ;; Don't need that ?
  ;; :local-variables (lambda (root) (lambda (root file) nil))
  :config-file ".eproject"

  ;; Other attributes/metadata

  ;; Example for CC Enhanced mode
  ;; :cflags ("-I.." "-I../utils")
  )

; -j4 disable unusedFunction
(defvar cppcheck-command "cppcheck --template gcc --enable=all "
  "The default command for running cppcheck.")

;; FIXME: Ugly
(defun init-sarcasm-cc-project ()
  "Initialize a project, set the compile command, the cppcheck
command, etc."
  (let ((make-cmd "make -B -j4 -k "))
    (set (make-local-variable 'compile-command)
         (if (file-exists-p "Makefile")
             make-cmd
           (format "cd %s; %s" (eproject-root) make-cmd))))

  (local-set-key [f6] (lambda ()
                        (interactive)
                        (let* ((compilation-read-command nil)
                               (flags (if (fboundp 'cc-enhanced-get-cflags)
                                          (cc-enhanced-get-cflags)))
                               (compile-command (concat (unless (file-exists-p "Makefile")
                                                          (format "cd %s; " (eproject-root)))
                                                        (format "%s %s ." cppcheck-command
                                                                (mapconcat 'identity flags " ")))))
                          (call-interactively 'compile)))))

(add-hook 'sarcasm-cc-project-file-visit-hook 'init-sarcasm-cc-project)

(provide 'sarcasm-cc-project)
