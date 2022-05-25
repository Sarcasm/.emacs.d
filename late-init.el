;;; late-init.el --- Late Emacs user configuration file  -*- lexical-binding: t; -*-

;; The late initialization happens just after the startup buffers have been shown.
;; It's a good place to configure things that do not affect how the buffer is presented.
;; For example, enabling global minor modes that impact editing (as opposed to viewing),
;; these cannot be autoloaded and will take some time to load.
;;
;; Unlike early-init.el and init.el,
;; this late-init.el is not a standard Emacs initialization step.

;; Automatically 'chmod' scripts as they are saved
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(column-number-mode)
(context-menu-mode)
(delete-selection-mode)
(electric-layout-mode)
(electric-pair-mode)
(fido-vertical-mode)
(global-auto-revert-mode)
(global-hl-line-mode)
(global-subword-mode)
(savehist-mode)
(winner-mode)

(with-eval-after-load 'company
  (global-company-mode)
  (define-key company-mode-map "\M-\r" #'company-complete))
(require 'company nil t)

(with-eval-after-load 'flymake
  ;; Configure `display-buffer' behaviour for some special buffers.
  ;; see http://www.lunaryorn.com/2015/04/29/the-power-of-display-buffer-alist.html
  ;; and https://github.com/lunaryorn/.emacs.d/blob/2233f7dc277453b7eaeb447b00d8cb8d72435318/init.el#L420-L439
  (add-to-list 'display-buffer-alist
               '("\\`\\*Flymake diagnostics"
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.22)))
  (define-key mode-specific-map "!p" #'flymake-show-project-diagnostics)
  (define-key mode-specific-map "!l" #'flymake-show-buffer-diagnostics))

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
  (autoload 'sarcasm-move-text-up "sarcasm" nil t)
  (define-key global-map [M-up] #'sarcasm-move-text-up)
  (autoload 'sarcasm-move-text-down "sarcasm" nil t)
  (define-key global-map [M-down] #'sarcasm-move-text-down))

(progn ;; muliple-cursors
  (define-key global-map [(control \;)] #'mc/mark-all-dwim)
  (define-key global-map [C-S-mouse-1] #'mc/add-cursor-on-click))

(progn ;; ibuffer
  ;; Thx: http://martinowen.net/blog/2010/02/tips-for-emacs-ibuffer.html
  (defun sarcasm-ibuffer-other-window ()
    "Open ibuffer in other window."
    (interactive)
    (ibuffer t))

  (defun sarcasm-ibuffer-mode-init ()
    (ibuffer-auto-mode 1)                 ;auto update
    (ibuffer-switch-to-saved-filter-groups "default"))

  (define-key global-map [remap list-buffers] #'sarcasm-ibuffer-other-window)

  (add-hook 'ibuffer-mode-hook #'sarcasm-ibuffer-mode-init)

  (with-eval-after-load 'ibuffer
    (add-to-list 'ibuffer-formats '(mark
                                    modified
                                    " "
                                    (name 25 25 :left :elide)
                                    " "
                                    (size 9 -1 :right)
                                    " "
                                    (mode 16 16 :left :elide)
                                    " " filename-and-process))
    (setq ibuffer-saved-filter-groups
          '(("default"

             ("Interactive" (or (mode . lisp-interaction-mode)
                                (name . "\*Messages\*")
                                (name . "\*compilation\*")
                                (name . "\*Customize\*")
                                (name . "\*ag search\*")
                                (name . "\*grep\*")))

             ("Dired" (mode . dired-mode))

             ;; Need to be before "Programming" otherwise
             ;; `emacs-lisp-mode' will match.
             ("Emacs config" (filename . ".config/emacs"))

             ("Org-Mode" (mode . org-mode))

             ("Programming" (derived-mode . prog-mode))

             ("Magit" (name . "\*magit"))

             ("Help" (or (name . "\*Help\*")
                         (name . "\*Apropos\*")
                         (name . "\*info\*")))

             ("Man" (mode . Man-mode)))))))

(with-eval-after-load 'ledger-mode
  (autoload 'sarcasm-ledger-time-stamp "sarcasm-ledger")
  (define-key mode-specific-map "." #'sarcasm-ledger-time-stamp))

(let ((curtime (current-time)))
  (message "init:%dms total:%dms[+%dms] gc-done:%d"
	   (* 1000 (float-time (time-subtract after-init-time before-init-time)))
	   (* 1000 (float-time (time-subtract curtime before-init-time)))
	   (* 1000 (float-time (time-subtract curtime after-init-time)))
	   gcs-done))
