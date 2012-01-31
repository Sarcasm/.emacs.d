;; Window manipulations -- Guillaume Papin
;; usage:
;; (require 'sarcasm-winresize)
;; Mode for window manipulations
;; Source: http://www.emacswiki.org/emacs/WindowResize
;; Modified by me

(defconst win-resize-value 4
 "The number of lines or columns for a resizement operation.")

(define-key mode-specific-map (kbd "w") 'iresize-mode)
(global-set-key [M-S-up]      'win-resize-up)
(global-set-key [M-S-down]    'win-resize-down)
(global-set-key [M-S-left]    'win-resize-left)
(global-set-key [M-S-right]   'win-resize-right)

(defvar iresize-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m [up]		  'win-resize-up)
    (define-key m [down]	  'win-resize-down)
    (define-key m [left]	  'win-resize-left)
    (define-key m [right]	  'win-resize-right)
    (define-key m (kbd "C-c C-c") 'iresize-mode)
    (define-key m (kbd "C-g")	  'iresize-mode)
    (define-key m (kbd "SPC")	  'iresize-mode)
    (define-key m (kbd "RET")	  'iresize-mode)
    (define-key m (kbd "=")	  (lambda ()
                                    (interactive)
                                    (enlarge-window-horizontally win-resize-value)))
    (define-key m (kbd "-")	  (lambda ()
                                    (interactive)
                                    (enlarge-window-horizontally (- win-resize-value))))
    (define-key m (kbd "+")	  (lambda ()
                                    (interactive)
                                    (enlarge-window win-resize-value)))
    (define-key m (kbd "_")	  (lambda ()
                                    (interactive)
                                    (enlarge-window (- win-resize-value))))
    m)
  "Keymap for `iresize-mode'.")

(define-minor-mode iresize-mode
  "Minor mode for window manipulations aka \"resizement\"."
  :initial-value nil
  :lighter " IResize"
  :keymap iresize-mode-map
  :group 'iresize)

;; Window "resizement", work only when a window touch two edges of the
;; frame.
(defun win-resize-top-or-bot ()
  "Figure out if the current window is on top, bottom or in the
middle"
  (let* ((win-edges (window-edges))
	 (this-window-y-min (nth 1 win-edges))
	 (this-window-y-max (nth 3 win-edges))
	 (fr-height (frame-height)))
    (cond
     ((eq 0 this-window-y-min) "top")
     ((eq (- fr-height 1) this-window-y-max) "bot")
     (t "mid"))))

(defun win-resize-left-or-right ()
  "Figure out if the current window is to the left, right or in
the middle"
  (let* ((win-edges (window-edges))
	 (this-window-x-min (nth 0 win-edges))
	 (this-window-x-max (nth 2 win-edges))
	 (fr-width (frame-width)))
    (cond
     ((eq 0 this-window-x-min) "left")
     ((eq fr-width this-window-x-max) "right")
     ;; X is pig-headed
     ((when window-system
	(eq (+ fr-width 2) this-window-x-max) "right")) ;doesn't work
     (t "mid"))))

(defun win-resize-up ()
  "Resize the window in the North direction."
  (interactive)
  (cond
   ((equal "top" (win-resize-top-or-bot)) (enlarge-window (- win-resize-value)))
   ((equal "bot" (win-resize-top-or-bot)) (enlarge-window win-resize-value))
   ((equal "mid" (win-resize-top-or-bot)) (enlarge-window (- win-resize-value)))
   (t (message "nil"))))

(defun win-resize-down ()
  "Resize the window in the South direction."
  (interactive)
  (cond
   ((equal "top" (win-resize-top-or-bot)) (enlarge-window win-resize-value))
   ((equal "bot" (win-resize-top-or-bot)) (enlarge-window (- win-resize-value)))
   ((equal "mid" (win-resize-top-or-bot)) (enlarge-window win-resize-value))
   (t (message "nil"))))

(defun win-resize-left ()
  "Resize the window in the East direction."
  (interactive)
  (cond
   ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally (- win-resize-value)))
   ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally win-resize-value))
   ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally (- win-resize-value)))))

(defun win-resize-right ()
  "Resize the window in the West direction."
  (interactive)
  (cond
   ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally win-resize-value))
   ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally (- win-resize-value)))
   ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally win-resize-value))))

(provide 'sarcasm-winresize)
