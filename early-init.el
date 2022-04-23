;;; early-init.el -*- lexical-binding: t; -*-

;; Bump GC to at least 1.8MB.
(let ((normal-gc-threshold (max 1800800 gc-cons-threshold)))
  ;; boost threshold on startup
  (setq gc-cons-threshold 100000000)

  ;; schedule this early because `run-with-idle-timer' seems to run in reverse order,
  ;; so hopefully this will run last
  (run-with-idle-timer
   0 nil
   #'(lambda ()
       ;; load filename sans extension so that `load' tries byte compile init files
       (load (concat user-emacs-directory "late-init") nil t nil t)

       ;; restore the default GC threshold
       (setq gc-cons-threshold normal-gc-threshold))))

(setq package-quickstart t)

;; Load UI variables and theme early,
;; to remove potential glitches on startup
;; such as background color change, menu-bar show -> hide, ...
(load-theme 'sarcasm t)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; And set these to nil so users don't have to toggle the modes twice to
;; reactivate them.
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

(setq frame-resize-pixelwise t)

;; Ignore .Xresources
;;
;; This avoid an issue with buggy cursor color on KDE,
;; when using emacsclient:
;; - https://lists.gnu.org/archive/html/help-gnu-emacs/2002-04/msg00116.html
;; - https://emacs.stackexchange.com/a/32764/901
;;
;; Alternatively, one could fix the issue by commenting
;; the offending Emacs*Foreground property
;; defined in `/usr/share/kdisplay/app-defaults/Emacs.ad`:
;;
;; ```diff
;; -Emacs*Foreground:		WINDOW_FOREGROUND
;; +! Emacs*Foreground:		WINDOW_FOREGROUND
;; ```
;;
;; Using sed:
;;     sudo sed -i 's/Emacs\*Foreground:/! Emacs*Foreground:/g' /usr/share/kdisplay/app-defaults/Emacs.ad
(advice-add #'x-apply-session-resources :override #'ignore)
