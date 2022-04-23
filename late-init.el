;;; late-init.el --- Late Emacs user configuration file  -*- lexical-binding: t; -*-

;; The late initialization happens just after the startup buffers have been shown.
;; It's a good place to configure things that do not affect how the buffer is presented.
;; For example, enabling global minor modes that impact editing (as opposed to viewing),
;; these cannot be autoloaded and will take some time to load.
;;
;; Unlike early-init.el and init.el,
;; this late-init.el is not a standard Emacs initialization step.

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(with-eval-after-load 'package
  (when (< emacs-major-version 28)
    (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/")))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (setq package-archive-priorities '(("gnu" . 20)
				     ("nongnu" . 10)
				     ("melpa" . 0))))

;; Automatically 'chmod' scripts as they are saved
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(column-number-mode)
(delete-selection-mode)
(electric-layout-mode)
(electric-pair-mode)
(global-auto-revert-mode)
(global-hl-line-mode)
(global-subword-mode)
(savehist-mode)
(winner-mode)

(with-eval-after-load 'vertico
  (vertico-mode))
(require 'vertico nil t)

(with-eval-after-load 'company
  (global-company-mode)
  (define-key company-mode-map "\M-\r" #'company-complete))
(require 'company nil t)

(defun sarcasm-narrow-to-region (start end)
  "Deactivate the mark after `narrow-to-region'."
  (interactive "r")
  (narrow-to-region start end)
  (deactivate-mark))

(define-key global-map [remap narrow-to-region] #'sarcasm-narrow-to-region)

(progn ;; windows
  (define-key global-map [S-up] #'windmove-up)
  (define-key global-map [S-down] #'windmove-down)
  (define-key global-map [S-left] #'windmove-left)
  (define-key global-map [S-right] #'windmove-right)

  (define-key global-map [C-S-up] #'buf-move-up)
  (define-key global-map [C-S-down] #'buf-move-down)
  (define-key global-map [C-S-left] #'buf-move-left)
  (define-key global-map [C-S-right] #'buf-move-right)

  (autoload 'win-resize-up "sarcasm-winresize" nil t)
  (autoload 'win-resize-down "sarcasm-winresize" nil t)
  (autoload 'win-resize-left "sarcasm-winresize" nil t)
  (autoload 'win-resize-right "sarcasm-winresize" nil t)
  (define-key global-map [M-S-up] #'win-resize-up)
  (define-key global-map [M-S-down] #'win-resize-down)
  (define-key global-map [M-S-left] #'win-resize-left)
  (define-key global-map [M-S-right] #'win-resize-right)

  (autoload 'iresize-mode "sarcasm-winresize" nil t)
  (define-key mode-specific-map "w" #'iresize-mode)

  (define-key global-map [remap other-window] #'ace-window))

(progn ;; misc keybindings
  (define-key ctl-x-map "c" #'whitespace-cleanup)

  (autoload 'sarcasm-find-file-as-root "sarcasm" nil t)
  (define-key ctl-x-map "F" #'sarcasm-find-file-as-root)
  (autoload 'sarcasm-dired-user-emacs-directory "sarcasm" nil t)
  (define-key global-map [f5] #'sarcasm-dired-user-emacs-directory)

  ;; Found on Stack Overflow
  ;; http://stackoverflow.com/questions/2091881/emacs-font-sizing-with-ctrl-key-and-mouse-scroll/2092158#2092158
  (define-key global-map [C-mouse-4] #'text-scale-increase)
  (define-key global-map [C-mouse-5] #'text-scale-decrease)

  ;; M-<up> and M-<down> like the Ecplise IDE functionnality
  (autoload 'sarcasm-move-text-down "sarcasm" nil t)
  (define-key global-map [M-up] #'sarcasm-move-text-up)
  (autoload 'sarcasm-move-text-down "sarcasm" nil t)
  (define-key global-map [M-down] #'sarcasm-move-text-down))

(progn ;; muliple-cursors
  (define-key global-map [(control \;)] #'mc/mark-all-dwim)
  (define-key global-map [C-S-mouse-1] #'mc/add-cursor-on-click))

(with-eval-after-load 'ledger-mode
  (autoload 'sarcasm-ledger-time-stamp "sarcasm-ledger")
  (define-key mode-specific-map "." #'sarcasm-ledger-time-stamp))
