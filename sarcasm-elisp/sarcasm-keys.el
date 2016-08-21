;; Global keybindings -- Guillaume Papin
;; usage:
;; (require 'sarcasm-keys)

;; Few examples of key names:
;; http://xahlee.org/emacs/keystroke_rep.html
;; Show all keys with M-x RET describe-bindings
;;
;; Free keys:
;; <f5>, ..., <f9>
;; C-c <any letter (not C-..., maybe M- ?)>
;;
;; Sources:
;; - http://ubuntuforums.org/showpost.php?p=8943092&postcount=3
;; - http://superuser.com/questions/214295/emacs-how-to-choose-good-custom-key-bindings
(global-set-key [f5]
                (lambda (arg)
                  ;; Open Emacs user configuration directory in dired.
                  ;; With a prefix argument open the file containing
                  ;; the el-get configuration.
                       (interactive "P")
                       (if arg
                           (find-file (concat *sarcasm-directory*
                                              "sarcasm-el-get.el"))
                         (dired *sarcasm-directory*))))

;; (define-key mode-specific-map (kbd "i") 'imenu)
;; xscope has [C-c s] for prefix
;; (global-set-key (kbd "C-c s") 'shell)
(define-key mode-specific-map (kbd "b") 'sarcasm-serial-term)
(global-set-key (kbd "C-x c") 'whitespace-cleanup)
(define-key mode-specific-map (kbd "f") 'folding-mode)
(define-key mode-specific-map [?i] 'sarcasm-path-to-kill-ring)

;; Found on Stack Overflow
;; http://stackoverflow.com/questions/2091881/emacs-font-sizing-with-ctrl-key-and-mouse-scroll/2092158#2092158
(global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
(global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)

;; For compilation buffer
(global-set-key (kbd "M-n") (lambda ()
                              (interactive)
                              (if flymake-mode
                                  (flymake-goto-next-error)
                                (next-error))))

(global-set-key (kbd "M-p")  (lambda ()
                               (interactive)
                               (if flymake-mode
                                   (flymake-goto-prev-error)
                                 (previous-error))))

;; Irony point/mark
(define-key mode-specific-map (kbd "8 i")
  (lambda ()
    (interactive)
    ;; #x2E2E in hexa or REVERSED QUESTION MARK
    (ucs-insert 11822)))

;; AHG
(define-key mode-specific-map (kbd "x a") 'ahg-status)

(define-key mode-specific-map (kbd "x f") 'flyspell-prog-mode) ;FIXME: maybe flyspell-mode ?

;; Make window switching a little easier. C-x-o is a pain.
;; Easy window switching with M-x windmove-{left,right,down,up} RET
;; (windmove-default-keybindings 'meta) (meta, control, shift)
(windmove-default-keybindings)		; Shit + arrows (by default)

;; Align with keyboard !
(define-key mode-specific-map (kbd "a") 'align-region-or-current)
(define-key mode-specific-map (kbd "A") 'align-regexp)

;; M-<left> and M-<right> like the Ecplise IDE functionnality
(define-key esc-map [up]   'move-text-up)
(define-key esc-map [down] 'move-text-down)
(global-set-key (kbd "M-<up>")   'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)

;; SLIME / StumpWM binding
(define-key mode-specific-map (kbd "x s") 'slime-stumpwm-repl)

(define-key mode-specific-map (kbd "x \"")
  (lambda (arg begin end)
    (interactive "*P\nr")
    (if arg
        (sarcasm-unescape-quotes begin end)
      (sarcasm-escape-quotes begin end))))

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

;; or some other keybinding...
(global-set-key (kbd "C-x F") 'sarcasm-find-file-as-root)

(provide 'sarcasm-keys)
