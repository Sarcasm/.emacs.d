;; Utility functions -- Guillaume Papin
;; usage:
;; (require 'sarcasm-utils)

(defun align-region-or-current ()
  "If a region is active align the region, otherwise align at
point."
  (interactive)
  (if mark-active
      (align (region-beginning) (region-end))
    (align-current)))

(defun c-man (NAME)
  "Find the C manual page corresponding to the function NAME.
Search for man(2) and man(3) by default."
  (if (file-exists-p (concat "/usr/share/man/man2/" NAME ".2.gz"))
      (man (concat NAME "(2)"))
    (if (file-exists-p (concat "/usr/share/man/man3/" NAME ".3.gz"))
        (man (concat NAME "(3)"))
      (man NAME))))

(defun c-man-at-point ()
  "Find a C man page with the current word if present, otherwise
require input from user."
  (interactive)
  (when (not (setq cur-word (current-word)))
    (setq cur-word (read-from-minibuffer "Man Page: ")))
  (if (string= "" cur-word)
      (message "No man args given")
    (c-man cur-word)))

;; Miscellaneous functions for compilation
(defun flymake-or-compile-next-error ()
  "If Flymake mode is enable then go to the next Flymake error,
otherwise assume it's compile next error."
  (interactive)
  (if flymake-mode
      (flymake-goto-next-error)
    (next-error)))

(defun flymake-or-compile-prev-error ()
  "If Flymake mode is enable then go to the previous Flymake error,
otherwise assume it's compile previous error."
  (interactive)
  (if flymake-mode
      (flymake-goto-prev-error)
    (previous-error)))

;; Source: http://groups.google.com/group/gnu.emacs.help/browse_thread/thread/75dd91fd45742d54?pli=1
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(defun slime-stumpwm-repl ()
  "Start SLIME and open the StumpWM directory."
  (interactive)
  ;; (autopair-mode nil)                 ;due to post-command-hook issues
  (let* ((stumpwm-config-dir "~/.stumpwm.d/")
         (slime-stumpwm-buffer-name "*slime-repl sbcl*")
         (stumpwm-buffer (get-buffer slime-stumpwm-buffer-name)))
    (find-file-other-window stumpwm-config-dir)
    (if stumpwm-buffer
        (switch-to-buffer stumpwm-buffer)
      (slime-connect "127.0.0.1" "4005")
      ;; ~After the animation
      (run-at-time 4 nil (lambda ()
                           (slime-repl-set-package "stumpwm")
                           (slime-cd stumpwm-config-dir))))))

(defun fixme-and-todo-font-lock ()
  "Add a coloration for TODO: and FIXME: keywords."
  (font-lock-add-keywords nil
                          '(("\\<\\(FIXME\\):" 1 font-lock-warning-face t)))
  (font-lock-add-keywords nil
                          '(("\\<\\(TODO\\):" 1 font-lock-keyword-face t))))

;; W window urgent hint handling
;; source: http://www.linux.org.ru/forum/development/4076070
;; usage:
;; urgent [ON]:
;; (x-urgent-hint (selected-frame) t)
;; urgent [OFF]
;; (x-urgent-hint (selected-frame) nil)
(defun x-wm-hints (frame &optional source)
  (mapcar #'(lambda (field)
             (if (consp field)
                 (+ (lsh (car field) 16) (cdr field))
               field))
          (x-window-property
           "WM_HINTS" frame "WM_HINTS"
           (if source
               source
             (string-to-number (frame-parameter frame 'outer-window-id)))
           nil t)))

(defun x-urgent-hint (frame arg)
  (let* ((wm-hints (x-wm-hints frame))
         (flags (car wm-hints)))
    (setcar wm-hints (if arg
                         (logior flags #x00000100)
                       (logand flags #xFFFFFEFF)))
    (x-change-window-property "WM_HINTS" wm-hints frame "WM_HINTS" 32 t)))

(defun sarcasm-path-to-kill-ring (&optional absolute)
  "Save the current path of the buffer into the kill ring.

With a prefix argument give the absolute path (symlink
resolved)."
  (interactive "P")
  ;; Pathname stuff stolen from `eproject--buffer-file-name'.
  (let ((pathname (or (buffer-file-name)
                      (and (eq major-mode 'dired-mode)
                           (expand-file-name (if (consp dired-directory)
                                                 (car dired-directory)
                                               dired-directory))))))
    (if (not pathname)
        (message "No filename associated with this buffer.")
      (let ((final-path (if absolute
                            (or (file-truename pathname) pathname)
                          pathname)))
        (kill-new final-path)
        (message "\"%s\" added to kill ring" final-path)))))

;; From kde-emacs
;; file : kde-emacs-utils.el
;; Makes ',' insert ', '
;; Note: Might be interesting to use something similar to
;;       `c-scope-operator' who use `insert-and-inherit'.
(defun sarcasm-insert-comma (arg)
  (interactive "*P")
  (let* ((ch (char-after))
         (spacep (not (or (eq ch ? )
                          (c-in-literal)
                          arg))))
    (self-insert-command (prefix-numeric-value arg))
    (if spacep
	(insert " "))))

(defun sarcasm-escape-quotes (start end)
  "Add a backspace before each quote found in region."
  (interactive "*r")
  (goto-char start)
  (while (search-forward "\"" end t)
    (setq end (+ end 1))
    (replace-match "\\\"" nil t)))

(defun sarcasm-unescape-quotes (start end)
  "Replace each escaped quote in the region with a simple quote."
  (interactive "*r")
  (goto-char start)
  (while (search-forward "\\\"" end t)
    (setq end (- end 1))
    (replace-match "\"" nil t)))

;; http://stackoverflow.com/a/998472/951426
(defun sarcasm-duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")
  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))
  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion
      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))
      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (insert "\n")         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )
      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let
  ;; put the point in the lowest line and return
  (next-line arg))

(provide 'sarcasm-utils)
