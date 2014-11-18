;; Serial Terminal stuff -- Guillaume Papin
;; usage:
;; (require 'sarcasm-serial-term)
;;
;; Note:
;; - http://lofi.jet-age.co.nz/2011/06/stopping-bash-from-wrapping-lines-in-picocom-with-a-wide-terminal/
;; - http://unix.stackexchange.com/a/61608/88925

;; For serial port, default to 115200 b/s instead of 9600
(setq serial-speed-history
      '("115200" ;; Given twice because 115200 b/s is the most common speed
        "1200" "2400" "4800" "9600" "14400" "19200"
        "28800" "38400" "57600" "115200"))

(defun sarcasm-serial-term ()
  (interactive)
  (serial-term "/dev/ttyUSB0" 115200)
  ;; eterm-color is the TERM used by Emacs when using `M-x term RET'
  ;;
  ;; On Debian-based systems you administer, you can apt-get install
  ;; ncurses-term, which includes /usr/share/terminfo/e/eterm-color.
  (term-send-string (get-buffer-process (current-buffer))
                    "export TERM=eterm-color\nclear\n"))

;; pre-condition is to be in a serial term process buffer
(defun sarcasm-serial-term-update-window-size ()
  "Change process window size."
  ;; effectively calls ioctl(TIOCSWINSZ ...)
  (set-process-window-size (get-buffer-process (current-buffer))
                           (window-height)
                           (window-width))
  ;; this is sad but we can't actually do this:
  ;;     (signal-process (get-buffer-process (current-buffer)) 'winch)
  ;; See Info node `(elisp)Serial Ports':
  ;;     ...
  ;;     A serial process object has no process ID, however, and you can't send
  ;;     signals to it, and the status codes are different from other types of
  ;;     processes.
  ;;
  ;; telling the shell to monitor the window size doesn't work either
  ;;     shopt -s checkwinsize
  ;; http://unix.stackexchange.com/a/61608/88925
  ;;
  ;; What seems to work is to call 'resize ; clear' explicitely on the shell
  )

(defun sarcasm-serial-term-mode-hook ()
  (when (and (derived-mode-p 'term-mode)
             (eq (process-type nil) 'serial))
    ;; add this hook as buffer local, so it runs once per window as opposed to
    ;; once per frame
    ;;
    ;; -- http://stackoverflow.com/a/11255996/951426
    (add-hook 'window-configuration-change-hook
              'sarcasm-serial-term-update-window-size nil t)))

(add-hook 'shell-mode-hook 'sarcasm-serial-term-mode-hook)

(provide 'sarcasm-serial-term)
