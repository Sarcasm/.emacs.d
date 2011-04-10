;;; sarcasm-theme.el --- Custom face theme for Emacs

;; Copyright (C) 2010 Guillaume Papin.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;; usage:
;; In init file:
;; (setq custom-theme-directory sarcasm-load-path)
;; (load-theme 'sarcasm)
;; Or after init:
;; M-x `load-theme' RET sarcasm RET

;; Note:
;; http://elpa.gnu.org/themes/
;; http://git.naquadah.org/?p=naquadah-theme.git;a=blob_plain;f=naquadah-theme.el;hb=HEAD

;;; Code:

(deftheme sarcasm
  "Created 2011-04-10.")

(let* ((black "black")
       (white "white")
       (red "firebrick")
       (green "lime green")          ;lime green, green1, green2, green3 ?
       (blue "DodgerBlue2")
       (yellow "yellow2")
       (orange "orange")
       (pink "deep pink")
       (violet "maroon")
       (brown "saddle brown")
       (grey "gray30")
       (cyan "cornflower blue")

       (soft-black "grey10")
       (soft-white "white smoke") ;; "MistyRose2"
       (soft-blue "#4477aa")
       (soft-gold "DarkGoldenrod2")

       (dark-grey "grey17")
       (dark-red "dark red")

       (bright-grey "grey60")

       (background soft-black)
       (foreground soft-white))
  (custom-theme-set-faces
   'sarcasm
   `(default ((t (:background ,background :foreground ,foreground))))
   `(cursor ((t (:background ,soft-gold :foreground ,red :bold t))))
   `(region ((t (:background ,blue :foreground ,soft-white))))
   `(mode-line ((t (:background ,dark-grey :foreground ,bright-grey :italic t
                                :box (:line-width 1 :color ,grey)))))
   `(mode-line-inactive ((t (:background ,dark-grey :foreground ,bright-grey :italic nil  :box nil))))
   `(mode-line-buffer-id ((t (:bold t :foreground ,pink :italic nil))))
   `(header-line ((t (:background ,dark-red :foreground ,white))))
   `(fringe ((t (:background ,background))))
   `(minibuffer-prompt ((t (:foreground ,blue :bold t))))
   `(font-lock-builtin-face ((t (:foreground ,soft-blue)))) ;includes statements, elisp properties, ...
   `(font-lock-comment-face ((t (:foreground ,red :italic t))))
   `(font-lock-constant-face ((t (:foreground ,pink :bold t))))
   `(font-lock-function-name-face ((t (:foreground ,soft-blue :bold t))))
   `(font-lock-keyword-face ((t (:foreground ,cyan :bold t))))
   `(font-lock-string-face ((t (:foreground ,violet))))
   `(font-lock-type-face ((t (:foreground ,green :bold t))))
   `(font-lock-variable-name-face ((t (:foreground ,pink)))) ;define, variable name, ...
   `(font-lock-warning-face ((t (:foreground ,red :bold t)))) ;cwarn-mode
   `(isearch ((t (:background ,pink :foreground "black"))))
   `(lazy-highlight ((t (:background ,orange :foreground ,background))))
   `(link ((t (:foreground ,soft-blue :underline t))))
   `(link-visited ((t (:foreground ,blue :underline t))))
   `(button ((t (:foreground ,bright-grey :underline t)))) ;backtick links in lisp
   `(trailing-whitespace ((t (:background ,dark-grey))))

   ;; Flymake
   `(flymake-errline ((t (:background ,dark-grey :foreground ,soft-white :underline ,dark-red))))

   ;; C-x m
   `(message-header-name ((t (:foreground ,orange :bold t))))
   `(message-header-to ((t (:foreground ,pink :bold t))))
   `(message-header-other ((t (:foreground ,blue))))
   `(message-separator ((t (:foreground ,red))))
   `(message-header-subject ((t (:foreground ,green :bold t))))

   ;; ERC
   `(erc-prompt-face ((t (:background ,background :foreground ,orange :bold t))))

   ;; diff-mode
   `(diff-added ((t (:foreground ,green))))
   `(diff-changed ((t (:foreground ,orange))))
   `(diff-removed ((t (:foreground ,dark-red))))
   '(diff-hunk-header ((t (:bold t))))
   `(diff-function ((t (:foreground ,soft-blue :bold t))))
   `(diff-header ((t (:background ,dark-grey))))
   `(diff-file-header ((t (:foreground ,grey))))

   ;; magit
   '(magit-diff-add ((t (:inherit diff-added))))
   '(magit-diff-del ((t (:inherit diff-removed))))
   '(magit-diff-none ((t (:inherit diff-context))))
   `(magit-branch ((t (:foreground ,pink :bold t))))
   `(magit-header ((t (:foreground ,orange :bold t))))
   '(magit-diff-hunk-header ((t (:inherit (magit-header diff-hunk-header)))))
   '(magit-diff-file-header  ((t (:inherit (magit-header diff-file-header)))))
   `(magit-log-sha1 ((t (:foreground ,soft-blue))))
   `(magit-log-graph ((t (:foreground ,pink))))
   `(magit-item-highlight ((t (:background ,dark-red :foreground ,white))))
   `(magit-item-mark ((t (:foreground ,pink))))
   `(magit-log-tag-label ((t (:box t))))
   `(magit-log-head-label-bisect-good ((t (:background ,green :box t))))
   `(magit-log-head-label-bisect-bad ((t (:background ,red :box t))))
   `(magit-log-head-label-remote ((t (:background ,soft-gold, :box t))))
   '(magit-log-head-label-tags ((t (:inherit (magit-log-tag-label)))))
   `(magit-log-head-label-local ((t (:foreground ,soft-white :background ,dark-grey
                                                 :box t))))
   ))

(provide-theme 'sarcasm)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; sarcasm-theme.el  ends here
