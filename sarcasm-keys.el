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
(global-set-key (kbd "C-c s") 'shell)

;; Enable / Disable Fly{make,spell} mode
(when (fboundp 'flymake-mode)
  (global-set-key (kbd "M-RET") 'flymake-mode))
(global-set-key [f7] 'flyspell-prog-mode) ;FIXME: maybe flyspell-mode ?

;; Make window switching a little easier. C-x-o is a pain.
;; Easy window switching with M-x windmove-{left,right,down,up} RET
;; (windmove-default-keybindings 'meta) (meta, control, shift)
(windmove-default-keybindings)		; Shit + arrows (by default)

(provide 'sarcasm-keys)
