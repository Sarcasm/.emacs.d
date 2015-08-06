(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-highlighting-mode 'lines)

(require 'rx)

;; Configure `display-buffer' behaviour for some special buffers.
;; see http://www.lunaryorn.com/2015/04/29/the-power-of-display-buffer-alist.html
;; and https://github.com/lunaryorn/.emacs.d/blob/2233f7dc277453b7eaeb447b00d8cb8d72435318/init.el#L420-L439
(setq display-buffer-alist
      `(
        ;; Put REPLs and error lists into the bottom side window
        (,(rx bos (or "*Flycheck errors*" ; Flycheck error list
                      "*compilation"      ; Compilation buffers
                      "*Warnings*"        ; Emacs warnings
                      "*shell"            ; Shell window
                      ))
         (display-buffer-reuse-window
          display-buffer-in-side-window)
         (side            . bottom)
         (reusable-frames . visible)
         (window-height   . 0.33))
        ;; Let `display-buffer' reuse visible frames for all buffers.  This must
        ;; be the last entry in `display-buffer-alist', because it overrides any
        ;; later entry with more specific actions.
        ("." nil (reusable-frames . visible))))

(defun sarcasm-quit-bottom-side-windows ()
  "Quit windows at the bottom of the current frame."
  (interactive)
  (dolist (window (window-at-side-list nil 'bottom))
    (quit-window nil window)))

;; see available keys at window.el.gz's bottom
(define-key ctl-x-map "4" 'sarcasm-quit-bottom-side-windows)
