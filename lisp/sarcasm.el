;;; sarcasm.el --- Utility shared definitions

(defun sarcasm-deffered-global-mode (global-mode turn-on &optional mode)
  ;; call global-mode first, this one usually have autoload,
  ;; while turn-on doesn't
  (funcall global-mode)
  ;; then enable mode in existing buffers
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      ;; don't turn on the mode unnecessarily if it's already on
      (unless (symbol-value mode)
        (funcall turn-on)))))

;; Source: http://groups.google.com/group/gnu.emacs.help/browse_thread/thread/75dd91fd45742d54?pli=1
(defun sarcasm--move-text-internal (arg)
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

(defun sarcasm-move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (sarcasm--move-text-internal arg))

(defun sarcasm-move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (sarcasm--move-text-internal (- arg)))

;; http://emacs-fu.blogspot.fr/2013/03/editing-with-root-privileges-once-more.html
(defun sarcasm-find-file-as-root ()
  "Like `ido-find-file, but automatically edit the file with
root-privileges (using tramp/sudo), if the file is not writable by
user."
  (interactive)
  (let ((file (ido-read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))

(defun sarcasm-dired-user-emacs-directory ()
  (interactive)
  (dired user-emacs-directory))

(provide 'sarcasm)
;;; sarcasm.el ends here
