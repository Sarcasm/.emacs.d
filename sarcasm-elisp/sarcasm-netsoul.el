;; NetSoul stuff -- Guillaume Papin
;; usage:
;; (require 'sarcasm-netsoul)

(when (file-exists-p "~/pkg/elim/elisp")
  (add-to-list 'load-path "~/pkg/elim/elisp")
  (when (require 'garak nil t)

    ;; "prpl-bilboed-netsoul"
    ;; "netsoul" */

    ;; /add-account USERNAME PROTOCOL
    ;; /configure-account ACCOUNT
    ;; /register ACCOUNT
    ;; /connect USERNAME
    ;; /msg [ACCOUNT] BUDDY

    ;; /add-account papin_g prpl-bilboed-netsoul
    ;; /configure-account papin_g
    ;; /register papin_g
    ;; /connect papin_g
    ;; /msg papin_g papin_g ;*account* papin_g talk to /papin_g/
    ;; /add-buddy papin_g papin_g ;add papin_g to the buddy list
    ;; /part ;leave the conversation
    ;; /disconnect ;disconnect the account
    ;; /status available
    ;; /set-icon USERNAME /path/to/icon/image

    (require 'dbus)
    (defun send-desktop-notification (summary body timeout)
      "call notification-daemon method METHOD with ARGS over dbus"
      (dbus-call-method
       :session                                 ; use the session (not system) bus
       "org.freedesktop.Notifications"          ; service name
       "/org/freedesktop/Notifications"         ; path name
       "org.freedesktop.Notifications" "Notify" ; Method
       "emacs"
       0
       ""
       summary
       body
       '(:array)
       '(:array :signature "{sv}")
       ':int32 timeout))

    ;; FIXME: Unable to find how to create a notification function

    ;; ;; (defun notify-new-ns (&rest args)
    (defun notify-new-ns (process buffer ctype flags who title is-new text args)
      "Fonction appelée pour notifier d'un nouveau NetSoul."
      (interactive)
      ;; (x-urgent-hint (selected-frame) t)
      ;; (x-urgent-hint (car (frame-list)) t)
      (send-desktop-notification (concat "Elim: " title) (concat who " say " text) 10000)
      ;; (send-desktop-notification "Elim" (format "frame title: %s" (car (frame-list))) 10000)
      )

    ;; (notify-show-message :id       0
    ;;                      ::action-handler (lambda (a) (message "action: %S" a))
    ;;                      :body     "Time to Die."
    ;;                      :icon     "/home/vivek/src/elim/icons/garak.png"
    ;;                      :actions  '("some-action" "Action Button Label")
    ;;                      :hints    nil
    ;;                      :timeout  30000
    ;;                      :app-name "(elim . garak)"
    ;;                      :summary  "wake up!")


    ;; (notify-new-ns)

    ;; (add-to-list 'garak-alert-when ':hidden)
    ;; (setq garak-alert-methods '(notify-new-ns))


    ;; (defun ivan-elim-pop-buffer
    ;;   (process buffer ctype flags who title is-new text args)
    ;;   "Pop buffer on incoming messages."
    ;;   (pop-to-buffer buffer)
    ;;   (raise-frame (selected-frame)))

    ;; (setq garak-alert-methods '(ivan-elim-pop-buffer))

    (setq garak-hide-offline-buddies t)

    (global-set-key (kbd "C-c x n") 'garak)))

(provide 'sarcasm-netsoul)
