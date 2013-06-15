;;; Clang-format emacs integration for use with C/Objective-C/C++.

;; This defines a function clang-format-region that you can bind to a key.
;; A minimal .emacs would contain:
;;
;;   (load "<path-to-clang>/tools/clang-format/clang-format.el")
;;   (global-set-key [C-M-tab] 'clang-format-region)
;;
;; Depending on your configuration and coding style, you might need to modify
;; 'style' and 'binary' below.

;; TODO: look for the style recursively
;; TODO: auto-mode-alist ".clang_format / yaml???"

(defgroup clang-format nil
  "C, Objective-C and C++ auto formatting."
  :version "24.3"
  :group 'c)

(defcustom clang-format-executable (executable-find "clang-format")
  "Location of the clang-format executable."
  :group 'clang-format
  :require 'clang-format
  :type '(file :must-match t))

(defcustom clang-format-style "LLVM"
  "Style to use when formatting."
  :group 'clang-format
  :require 'clang-format
  :options '("LLVM" "Google" "Chromium" "Mozilla")
  :type '(string :tag "Preferred style"))
(make-variable-buffer-local 'clang-format-style)

;;;###autoload
(defun clang-format-region (beg end)
  (interactive "r")
  (if (null clang-format-executable)
      (message "%s" (concat "Couldn't find 'clang-format' executable."
                            " Please customize `clang-format-executable'."))
    (let* ((orig-windows (get-buffer-window-list (current-buffer)))
           (orig-window-starts (mapcar #'window-start orig-windows))
           (orig-point (point)))
      ;; TODO: escape narrowing?
      (call-process-region (point-min) (point-max)
			   clang-format-executable
			   t              ;delete the region
			   t nil
			   "-offset" (number-to-string (1- beg))
			   "-length" (number-to-string (- end beg))
			   "-style" clang-format-style)
    (goto-char orig-point)
    (dotimes (index (length orig-windows))
      (set-window-start (nth index orig-windows)
                        (nth index orig-window-starts))))))

(defun clang-format-current-line ()
  (interactive)
  (clang-format-region (min (point-at-bol) (1- (point-max))) (point-at-eol)))

(defun clang-format-dwim ()
  (interactive)
  (if mark-active
      (call-interactively 'clang-format-region)
    (clang-format-region (min (point-at-bol) (1- (point-max)))
                         (point-at-eol))))

;;;###autoload
(defun clang-format-buffer (&optional buffer)
  "Format the whole BUFFER (default to current buffer)."
  ;; TODO: Narrowing???
  (interactive "*")
  (with-current-buffer (or buffer (current-buffer))
    (clang-format-region (point-min) (point-max))))

(provide 'clang-format)
