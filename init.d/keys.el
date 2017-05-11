(require 'bind-key)
(require 'sarcasm)

(bind-keys
 ("C-x c" . whitespace-cleanup)
 ("C-x F" . sarcasm-find-file-as-root)
 ("<f5>" . sarcasm-dired-user-emacs-directory)
 ;; Found on Stack Overflow
 ;; http://stackoverflow.com/questions/2091881/emacs-font-sizing-with-ctrl-key-and-mouse-scroll/2092158#2092158
 ("<C-mouse-4>" . text-scale-increase)
 ("<C-mouse-5>" . text-scale-decrease)
 ;; M-<up> and M-<down> like the Ecplise IDE functionnality
 ("M-<up>" . sarcasm-move-text-up)
 ("M-<down>" . sarcasm-move-text-down))

(use-package sarcasm-winresize
  :bind (("C-c w" . iresize-mode)
         ("M-S-<up>" . win-resize-up)
         ("M-S-<down>" . win-resize-down)
         ("M-S-<left>" . win-resize-left)
         ("M-S-<right>" . win-resize-right)))
