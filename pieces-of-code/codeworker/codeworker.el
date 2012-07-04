(require 'cl)
(require 'ido)
(require 'codeworker-completion nil t)

(setq compilation-auto-jump-to-first-error nil)

;; Usage:
;; (add-to-list 'load-path "le/repertoire/de/codeworker.el/")
;; (require 'codeworker)

(let* ((path (locate-library "codeworker.el"))
       (kooc-path (if path (expand-file-name (concat (file-name-directory path) "..")))))
  (when (file-exists-p (concat kooc-path "/kooc"))
    (add-to-list 'exec-path kooc-path)
    (setenv "PATH" (concat (getenv "PATH") ":" kooc-path))
    (setenv "KOOC_PATH" kooc-path)))

(unless (string= (user-full-name) "Guillaume Papin")
  (ido-mode 1)
  ;; Dot directory first for `ido-find-file', enter to go in dired mode
  (setq ido-use-filename-at-point nil)
  ;; (setq ido-show-dot-for-dired t)
  (setq ido-enable-flex-matching t)
  ;; Allow the same buffer to be open in different frames
  (setq ido-default-buffer-method 'selected-window)
  (ido-mode 1)

  (setq browse-url-generic-program "google-chrome")
  (setq browse-url-browser-function '(("^file:" . browse-file-url)
                                      ("."      . browse-url-generic))))

(defvar codeworker-completion-alist nil
  "An alist of completion (\"function . url\")")

(defvar codeworker-completion-list nil)

(defun codeworker-decode-identifier (string)
  (loop with str = string
        for pair in '(("&nbsp;" . " ")
                      ("&lt;"   . "<")
                      ("&gt;"   . ">")
                      ("&amp;"  . "&"))
        do (setq str (replace-regexp-in-string (car pair) (cdr pair) str))
        finally return str))

(defun get-codeworker-completions ()
  (interactive)
  (unless codeworker-completion-alist
    (let (
          ;; (documentation (url-retrieve-synchronously
          ;;                 "http://codeworker.free.fr/Documentation.html"))
          ;; (documentation-url
          ;;  "http://codeworker.free.fr/manual_The_scripting_language.html#")

          (manual-idx (url-retrieve-synchronously
                       "http://codeworker.free.fr/manual__INDEX.html"))
          (manual-url-prefix
           "http://codeworker.free.fr/manual_.html"))

      ;; (save-excursion
      ;;   (set-buffer documentation)
      ;;   (goto-char (point-min))
      ;;   (while (re-search-forward "<CODE>\\(.+\\)</CODE>" nil t)
      ;;     (add-to-list 'codeworker-completion-alist (cons (match-string 1) (match-string 1))))
      ;;   (kill-buffer (current-buffer)))

      ;; #_appendedFile
      ;;
      ;; Url
      ;; http://codeworker.free.fr/manual__INDEX.html
      ;; match group:
      ;; 1 -> url
      ;; 2 -> identifier
      (save-excursion
        (set-buffer manual-idx)
        (goto-char (point-min))
        (while (re-search-forward "[^;]<a\\s-+href=\"manual_\\.html\\([^\"]+\\)\"[^>]+>\\([^<]+\\)" nil t)
          (let ((identifier (codeworker-decode-identifier (match-string 2))))
            (unless (assoc identifier codeworker-completion-alist) ;pas de duplicata
              (add-to-list 'codeworker-completion-alist
                           (cons identifier ;identifier
                                 (concat manual-url-prefix (match-string 1))))))) ;url
        (kill-buffer (current-buffer)))))
  (setq codeworker-completion-list (mapcar 'car codeworker-completion-alist))
  codeworker-completion-alist)

(defun codeworker-completing-read (prompt alist prefix)
  (let ((completion (ido-completing-read
                     prompt
                     (mapcar 'car alist)
                     nil ;ign
                     t ;require match
                     prefix)))
    (cdr (assoc completion alist))))

(defun codeworker-show-doc (arg)
  (interactive "P")
  (let ((url  (codeworker-completing-read "CodeWorker function: "
                                          (get-codeworker-completions)
                                          (if (null arg)(current-word)))))
    (browse-url url)))

(global-set-key [f7] 'codeworker-show-doc)

(add-to-list 'auto-mode-alist (cons "\\.cws" 'c++-mode))
(add-to-list 'auto-mode-alist (cons "\\.cwp" 'antlr-mode))
(add-to-list 'auto-mode-alist (cons "\\.k[ch]" 'objc-mode))

(when (featurep 'auto-complete)
  (add-hook 'antlr-mode 'auto-complete-mode)
  (ac-define-source codeworker
    '((init . get-codeworker-completions)
      (candidates . codeworker-completion-list)
      (requires . 2)
      (symbol . "s"))))

(defconst codeworker-kooc-keywords
  (cons
   (regexp-opt
    '("@import"
      "@module"
      "@member"
      "@virtual"
      "@auto"
      "@decltype"
      "@implementation") ;;'words
    )
   font-lock-keyword-face))

(defconst codeworker-tu-re
  "//\\s-+\\(?:CHECK:\\s-+\\(kooc\\|gcc\\)\\s-+\\(compile\\|error\\)\\|RETVAL:\\s-+-?[0-9]+\\)"
  "Regexp qui valide un fichier comme un fichier de test.")

(defun codeworker-tu-p ()
  "Si c'est un fichier de test unitaire pour le kooc."
  (and buffer-file-name
       (string-match "/test/" buffer-file-name)
       (string= (file-name-extension
                 (or (car-safe (nreverse (split-string buffer-file-name "/test/")))
                     ""))
                "kc")))

(defun codeworker-tu-update-header-line ()
  (setq header-line-format
        (unless (string-match
                 codeworker-tu-re
                 (buffer-substring-no-properties (point-min) (point-max)))
          "invalid valid test file (missing the test line CHECK|RETVAL)")))

(add-hook 'find-file-hook
          (lambda ()
            (when (or (string= (file-name-extension (buffer-file-name)) "cws")
                      (string= (file-name-extension (buffer-file-name)) "cwp"))
              (subword-mode 1)
              (when (featurep 'auto-complete)
                (auto-complete-mode)
                (add-to-list 'ac-sources 'ac-source-codeworker))
              (font-lock-add-keywords nil
                                      '(("\\<\\(FIXME\\):" 1 font-lock-warning-face t)))
              (font-lock-add-keywords nil
                                      '(("\\<\\(TODO\\):" 1 font-lock-keyword-face t))))
            (when (codeworker-tu-p)
              ;; Fichier de test kooc
              (codeworker-tu-update-header-line)
              (add-hook 'after-save-hook 'codeworker-tu-update-header-line nil t))
            ;;         (string= (file-name-extension (buffer-file-name)) "kh"))
            ;;     (font-lock-add-keywords nil (list
            ;;                                  codeworker-kooc-keywords)))
            ))

(font-lock-add-keywords 'objc-mode (list codeworker-kooc-keywords))

(require 'compile)
;; http://www.emacswiki.org/emacs/CreatingYourOwnCompileErrorRegexp
(add-to-list 'compilation-error-regexp-alist 'codeworker)
(add-to-list 'compilation-error-regexp-alist-alist
             '(codeworker
               "\\(\\(?:.*\\.cw[sp]\\)\\)(\\([[:digit:]]+\\)\\(?:,\\([[:digit:]]+\\)\\)?):"
               1 2 3))

;; (let ((buffer (get-buffer-create "/tmp/codeworker-functions.el")))
;;   (with-current-buffer buffer
;;     (erase-buffer)
;;     (insert "(setq codeworker-completion-alist '(")
;;     (dolist (function (delete-dups functions))
;;       (insert (format "\"%s\" " function)))
;;     (insert ")")))))

(provide 'codeworker)
