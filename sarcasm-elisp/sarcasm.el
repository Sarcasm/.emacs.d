;; Emacs init file -- Guillaume Papin
;; usage:
;; Load personnal config
;; (load (expand-file-name "~/.emacs.d/sarcasm-elisp/sarcasm.el"))

(defconst *sarcasm-load-path* "~/.emacs.d/sarcasm-elisp"
  "Default path for Sarcasm config files.")

;; Go to the projects root directory by default
(setq default-directory "/mnt/media/projects/")

;; I don't understand why, but the following doesn't work
;; ;; Change the font to your needs
;; (set-default-font "DejaVu Sans Mono-10")
;; So in ~/.Xdefaults / ~/.Xresources
;;
;; Emacs.FontBackend: xft
;; Emacs.font: DejaVu Sans Mono-10

(setq user-mail-address "guillaume.papin@epitech.eu"
      ;; user-full-name "Guillaume Papin"
      inhibit-startup-screen t	      ;do not display a startup screen
      autopair-autowrap t             ;wrap the region with the paired characters
      mouse-yank-at-point t           ;paste at cursor position
      scroll-preserve-screen-position t ;restore cursor after PgUp/PgDown

      ;; Sentences end with one space when M-q `fill-paragraph' is called
      sentence-end-double-space nil

      compilation-auto-jump-to-first-error t
      compilation-scroll-output t

      ;; C-k kills whole line and newline if at beginning of line
      kill-whole-line t)

(setq-default indent-tabs-mode nil	;remove tabulations
              ;; show-trailing-whitespace t
	      )

;; http://www.emacswiki.org/emacs/SavePlace
;; note: This is useful when =C-x C-v= is done
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/places")

;; Thanks http://www.emacswiki.org/emacs/JonathanArnoldDotEmacs
;; note: Slash for directory
(setq completion-ignored-extensions '(".o"
				      ".elc"
				      "~"
				      ".git/"
				      ".gitignore"
				      ".svn/"))

;; This sets the coding system priority and the default input method
;; and sometimes other things.
(set-language-environment "UTF-8")

;; (menu-bar-mode -1)
(tool-bar-mode -1) ;toolbar is visible by default on X emacs so disable it
(blink-cursor-mode -1)                  ;it's annoying
(scroll-bar-mode -1)
(column-number-mode 1)		 ;print column number on the mode-line
(winner-mode 1)
;; Replace selection
(delete-selection-mode 1)
;; Highlight current line
(global-hl-line-mode 1)

;; Whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes in the corresponding buffer, just revert its
;; content to reflect what's on-disk.
;; source: https://github.com/dimitri/emacs-kicker/blob/master/init.el
(global-auto-revert-mode 1)

;; Show the matching parenthesis
(require 'paren)
(show-paren-mode 1)

;; Emacs external `url browser' (usefull in Org-Mode)
(setq browse-url-generic-program "chromium-browser")
(setq browse-url-browser-function '(("^file:" . browse-file-url)
				    ("."      . browse-url-generic)))

 ;; Go to the next page automatically in Doc-View
(setq doc-view-continuous t)

; Automatically 'chmod' scripts as they are saved
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Gnus starting file
(setq gnus-init-file                    ;it will try the suffix .el[c]
      (concat (file-name-as-directory *sarcasm-load-path*) "sarcasm-gnus"))

;; Emacs perso load path
(add-to-list 'load-path "~/.emacs.d/elisp")

;; Add Sarcasm directory to default path
(add-to-list 'load-path *sarcasm-load-path*)

;; Narrowing is convenient
(put 'widen 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'org-narrow-to-subtree 'disabled nil)

(defadvice narrow-to-region (after narrow-to-region-unmark
activate compile)
  "Disable selection after `narrow-to-region'"
  (deactivate-mark)
)

(require 'sarcasm-el-get)               ;el-get packages and config
(require 'sarcasm-utils)		;utility functions
(require 'sarcasm-keys)			;global keybindings
(require 'sarcasm-rcirc)		;rcirc settings
(require 'sarcasm-org)			;Org-Mode settings
(require 'sarcasm-c-common)		;C/C++ common stuff
(require 'sarcasm-c)			;C stuff
(require 'sarcasm-c++)			;C++ stuff
(require 'sarcasm-flymake)              ;flymake stuff
(require 'sarcasm-makefile)             ;Makefile stuff
(require 'sarcasm-lua)                  ;Lua stuff
(require 'sarcasm-ruby)			;Ruby stuff
(require 'sarcasm-lisp)			;Lisp stuff
(require 'sarcasm-elpa)			;ELPA (Emacs Lisp Package Archive) stuff
(require 'sarcasm-gtags)                ;GTags stuff
(require 'sarcasm-semantic)             ;General Semantic stuff
(require 'sarcasm-ac)                   ;Auto-complete stuff
(require 'sarcasm-winresize)            ;window manipulations
(require 'sarcasm-interactively)        ;minibuffer completion & co.
;; (require 'sarcasm-session)           ;restoring Emacs at startup
(require 'sarcasm-backup)               ;backup files handling
(require 'sarcasm-dired)                ;dired stuff
(require 'sarcasm-insert)               ;auto-insert stuff
(require 'sarcasm-comment)              ;Comment settings
(require 'sarcasm-netsoul)              ;NetSoul stuff

;; Custom settings
(setq custom-file
      (concat (file-name-as-directory *sarcasm-load-path*) "sarcasm-custom.el"))
(load custom-file)

;; Color theme
(setq custom-theme-directory *sarcasm-load-path*)
(load-theme 'sarcasm)
