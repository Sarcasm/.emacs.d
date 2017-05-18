;;; init.el --- Emacs user configuration file  -*- lexical-binding: t; -*-

(let ((old-threshold gc-cons-threshold))
  (setq gc-cons-threshold 100000000)
  (run-with-idle-timer 1 nil (lambda ()
                               ;; restore the default GC threshold
                               (setq gc-cons-threshold old-threshold))))

;; stolen from https://github.com/tarsius/no-littering
(defconst sarcasm-etc-directory
  (expand-file-name (convert-standard-filename "etc/") user-emacs-directory)
  "The directory where packages place their configuration files.")

(defconst sarcasm-cache-directory
  (expand-file-name (convert-standard-filename ".cache/") user-emacs-directory)
  "The directory where packages place their persistent data files.")

(defun sarcasm-expand-etc-file-name (file)
  "Expand filename FILE relative to `sarcasm-etc-directory'."
  (let* ((path (expand-file-name (convert-standard-filename file) sarcasm-etc-directory)))
    (make-directory (file-name-directory path) t)
    path))

(defun sarcasm-expand-cache-file-name (file)
  "Expand filename FILE relative to `sarcasm-cache-directory'."
  (let* ((path (expand-file-name (convert-standard-filename file) sarcasm-cache-directory)))
    (make-directory (file-name-directory path) t)
    path))

(setq-default package-user-dir (sarcasm-expand-cache-file-name "elpa"))

(require 'package)
(setq package-enable-at-startup nil   ;Info node `(emacs) Package Installation'
      ;; prefer melpa-stable, then the default source gnu, then others
      package-archive-priorities '(("gnu"          . 10)
                                   ("melpa-stable" . 5))
      package-pinned-packages '((use-package . "melpa")))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;; NOTE: this call is probably the more expensive function call of the init file
;; but this is also the building block for the rest of the configuration
(package-initialize)

;; Configure and bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; ignore hidden files, this ignores lock files,
;; symlinks of the form .#foo.el -> user@hostname.XXXX:XXXXXX
(dolist (el-file (directory-files
                  (expand-file-name "init.d" user-emacs-directory) t
                  "^[^.].*\\.el$"))
  ;; load filename sans extension so that `load' tries byte compile init files
  (let ((load-prefer-newer t))
    ;; stolen from https://github.com/tarsius/no-littering
    (cl-letf (((symbol-function 'etc)
               (symbol-function #'sarcasm-expand-etc-file-name))
              ((symbol-function 'cache)
               (symbol-function #'sarcasm-expand-cache-file-name)))
      (load (file-name-sans-extension el-file) nil t))))
