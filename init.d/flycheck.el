(require 'sarcasm)

(eval-when-compile
  (require 'rx))

(use-package flycheck
  :ensure t
  :defer 3
  :functions global-flycheck-mode
  :preface (declare-function flycheck-mode-on-safe "ext:flycheck")
  :init (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :config (sarcasm-deffered-global-mode #'global-flycheck-mode
                                        #'flycheck-mode-on-safe
                                        'flycheck-mode))

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
         (window-height   . 0.22))
        ;; Let `display-buffer' reuse visible frames for all buffers.
        ;; This must be the last entry in `display-buffer-alist',
        ;; because it overrides any later entry with more specific actions.
        ("." nil (reusable-frames . visible))))
