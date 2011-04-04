;; Emacs init file -- Guillaume Papin
;; usage:
;; Load personnal config
;; (load (expand-file-name "~/.emacs.d/sarcasm-elisp/sarcasm.el"))

(defconst sarcasm-load-path "~/.emacs.d/sarcasm-elisp"
  "Default path for Sarcasm config files.")

(setq user-mail-address "guillaume.papin@epitech.eu"
      ;; user-full-name "Guillaume Papin"
      inhibit-startup-screen t	      ;do not display a startup screen
      )

(setq-default indent-tabs-mode nil	;remove tabulations
	      show-trailing-whitespace t
	      autopair-autowrap t ;wrap the region with the paired characters
	      gdb-many-windows t  ;use gdb-many-windows by default
	      sentence-end-double-space nil ;sentences end with one
					    ;space (when M-q
					    ;`fill-paragraph' is
					    ;called
	      mouse-yank-at-point t	    ;paste at cursor position
	      scroll-preserve-screen-position t	;restore cursor after PgUp/PgDown
	      )

;; Thanks http://www.emacswiki.org/emacs/JonathanArnoldDotEmacs
;; note: Slash for directory
(setq completion-ignored-extensions '(".o"
				      ".elc"
				      "~"
				      ".git/"
				      ".svn/"))

;; This sets the coding system priority and the default input method
;; and sometimes other things.
(set-language-environment "UTF-8")

(menu-bar-mode	-1)			;I rarely used that bar
(tool-bar-mode 0) ;toolbar is visible by default on X emacs so disable it
(column-number-mode 1)		 ;print column number on the mode-line
;; (display-time-mode 0)

;; Emacs perso load path
(add-to-list 'load-path "~/.emacs.d/elisp")

;; Add Sarcasm directory to default path
(add-to-list 'load-path sarcasm-load-path)

(require 'sarcasm-keys)			;global keybindings
(require 'sarcasm-rcirc)		;rcirc settings
(require 'sarcasm-org)			;Org-Mode settings
(require 'sarcasm-utils)		;utility functions
(require 'sarcasm-c-common)		;C/C++ common stuff
(require 'sarcasm-c)			;C stuff
(require 'sarcasm-c++)			;C++ stuff
(require 'sarcasm-elpa)			;ELPA (Emacs Lisp Package Archive) stuff
(require 'sarcasm-gtags)                ;GTags stuff
(require 'sarcasm-semantic)             ;General Semantic stuff
(require 'sarcasm-ac)                   ;Auto-complete stuff
(require 'sarcasm-el-get)               ;el-get packages and config
(require 'sarcasm-session)              ;restoring Emacs at startup
