;; Compilation stuff -- Guillaume Papin
;; usage:
;; (require 'sarcasm-compilation)

;; Stolen from http://stringofbits.net/2009/08/31/emacs-23-dbus-and-libnotify/
(require 'dbus)

(defun sarcasm-desktop-notify (summary body timeout &optional icon)
  "Display a desktop notification using DBus.
TITLE is the title of the notification and BODY the content.
TIMEOUT is an optional timeout to give in milliseconds (0 means
no timeout).

ICON if the name of an image under /usr/share/pixmaps or ~/.icons"
  (dbus-call-method
    :session                                 ;Use session (not system) bus
    "org.freedesktop.Notifications"          ;Service name
    "/org/freedesktop/Notifications"         ;Path name
    "org.freedesktop.Notifications" "Notify" ;Method
    "Emacs"                                  ;Application name
    0                                        ;Id
    (or icon "")                             ;Icon
    summary                                  ;Summary
    body                                     ;Body
    '(:array)
    '(:array :signature "{sv}")
    ':int32 timeout))

(defun sarcasm-compilation-notify (buffer message)
  (sarcasm-desktop-notify (buffer-name buffer) message 2000 "emacs.png"))

(setq compilation-finish-function 'sarcasm-compilation-notify)

(provide 'sarcasm-compilation)
