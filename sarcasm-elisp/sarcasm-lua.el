;; Lua stuff -- Guillaume Papin
;; usage:
;; (require 'sarcasm-lua)

(add-hook 'lua-mode-hook
          '(lambda ()
             (flymake-lua-load) ;enable Flymake
             (modify-syntax-entry ?_ "_") ;now '_' is a "symbol constituent"
             ))

(provide 'sarcasm-lua)
