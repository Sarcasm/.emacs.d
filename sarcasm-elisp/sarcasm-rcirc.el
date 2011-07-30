;; rcirc settings -- Guillaume Papin
;; usage:
;; (require 'sarcasm-rcirc)

(setq rcirc-default-nick "Sarcasm"
      rcirc-server-alist '(("irc.freenode.net"
                            :channels ("#emacs" "#wesnoth" "#wesnoth-dev"))))

(provide 'sarcasm-rcirc)
