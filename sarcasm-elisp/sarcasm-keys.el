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

(global-set-key (kbd "C-c i") 'imenu)
;; xscope has [C-c s] for prefix
;; (global-set-key (kbd "C-c s") 'shell)
(global-set-key (kbd "C-c b") 'shell)   ;b for bash...
(global-set-key (kbd "C-x c") 'whitespace-cleanup)
(global-set-key (kbd "C-c f") 'folding-mode)

;; For compilation buffer
(global-set-key (kbd "M-n") (lambda ()
                              (interactive)
                              (if flymake-mode
                                  (flymake-goto-next-error)
                                (next-error)
                                )))

(global-set-key (kbd "M-p")  (lambda ()
                              (interactive)
                              (if flymake-mode
                                  (flymake-goto-prev-error)
                                (previous-error)
                                )))

;; Magit
(global-set-key (kbd "C-c x m") 'magit-status)
;; Imagine:
;; LOCAL <-> SERVER
;; push: local -> server
;; pull: local <- server
;; remove '-' and...HEY !!! It's '>' and '<' !
(add-hook 'magit-mode-hook (lambda ()
                             (define-key magit-mode-map (kbd ">") 'magit-push)
                             (define-key magit-mode-map (kbd "<") 'magit-pull)))

;; Enable / Disable Fly{make,spell} mode
(when (fboundp 'flymake-mode)
  (global-set-key (kbd "M-RET") 'flymake-mode))

(global-set-key (kbd "C-c x f") 'flyspell-prog-mode) ;FIXME: maybe flyspell-mode ?

;; Make window switching a little easier. C-x-o is a pain.
;; Easy window switching with M-x windmove-{left,right,down,up} RET
;; (windmove-default-keybindings 'meta) (meta, control, shift)
(windmove-default-keybindings)		; Shit + arrows (by default)

;; Align with keyboard !
(global-set-key (kbd "C-c a") 'align-region-or-current)
(global-set-key (kbd "C-c A") 'align-regexp)

;; M-<left> and M-<right> like the Ecplise IDE functionnality
(define-key esc-map [up]   'move-text-up)
(define-key esc-map [down] 'move-text-down)
(global-set-key (kbd "M-<up>")	 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)

;; SLIME / StumpWM binding
(global-set-key (kbd "C-c x s") 'slime-stumpwm-repl)

(provide 'sarcasm-keys)
