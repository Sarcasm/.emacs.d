;; Emacs init file -- Guillaume Papin
;; usage:
;;
;; In the file "~/.emacs.d/init.el" (or "~/.emacs") put:
;;
;;    (load (concat user-emacs-directory
;;                  (file-name-as-directory "sarcasm-elisp")
;;                  "sarcasm.el"))
;;

(defconst *sarcasm-load-path* (concat user-emacs-directory "sarcasm-elisp")
  "Default path for Sarcasm config files.")

(defconst *sarcasm-directory* (file-name-as-directory *sarcasm-load-path*)
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
      kill-whole-line t

      save-abbrevs 'silently       ;don't want to answer yes everytime

      ;; Autosave each change)
      bookmark-save-flag 1

      ;; Use Window system clipboard
      x-select-enable-clipboard t
      yank-pop-change-selection t)

(setq-default indent-tabs-mode nil	;remove tabulations
              ;; show-trailing-whitespace t
	      )

;; http://www.emacswiki.org/emacs/SavePlace
;; note: This is useful when =C-x C-v= is done
(setq-default save-place t)
(require 'saveplace)
(setq save-place-file (concat user-emacs-directory "places"))

;; Thanks http://www.emacswiki.org/emacs/JonathanArnoldDotEmacs
;; note: Slash for directory
(setq completion-ignored-extensions '(".o" ".a" ".so"
				      ".elc"
                                      ".class" ".dll"
				      "~"
				      ".git/"
				      ".gitignore"
				      ".hg/"
				      ".hgignore"
				      ".svn/"))

(defconst sarcasm-ignored-files '("GPATH" "GRTAGS" "GTAGS"
                                  ".git" ".gitignore"
                                  ".hg" ".hgignore"
                                  ;; core.clj was ignored in `dired-omit-files'
                                  ;; "core"
                                  "vgcore"
                                  ".newsrc-dribble")
  "A list of filename and directory to ignore (no directory
separator should be involved).

note: at this time this variable is used for making the
`dired-omit-files' and some `eproject' project type.")

(defconst sarcasm-ignored-files-re '("cscope\\.\\w+"
                                     ;; coredump and valgrind coredump
                                     "\\(?:\\vg\\)?core\\.[[:digit:]]+")
  "See `sarcasm-ignored-files'.

There only difference is that each filename should be a regexp.")

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

;; Since Emacs 24
;; stolen from: https://github.com/bbatsov/emacs-dev-kit/blob/master/misc-config.el
(if (>= emacs-major-version 24)
    (progn
      (electric-pair-mode t)
      (electric-indent-mode t)
      ;; Example, insert a newline after semicolon
      ;; (setq electric-layout-rules '((?; . after)))
      (electric-layout-mode t))
  ;; ;; autopair mode
  ;; (require 'autopair)
  ;; (autopair-global-mode)
  ;; (setq autopair-autowrap t)
  ;; (setq autopair-blink nil)
  )

;; Highlight current line
(if (boundp 'global-hl-line-sticky-flag) ;introduced in emacs 24.1
    (setq global-hl-line-sticky-flag t))
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
(setq browse-url-generic-program "chromium")
(setq browse-url-browser-function '(("^file:" . browse-file-url)
				    ("."      . browse-url-generic)))

 ;; Go to the next page automatically in Doc-View
(setq doc-view-continuous t)

; Automatically 'chmod' scripts as they are saved
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Emacs perso load path
(add-to-list 'load-path (concat user-emacs-directory "elisp"))

;; Add Sarcasm directory to default path
(add-to-list 'load-path *sarcasm-load-path*)

;; Narrowing is convenient
(put 'widen 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'org-narrow-to-subtree 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(defadvice narrow-to-region (after narrow-to-region-unmark
activate compile)
  "Disable selection after `narrow-to-region'"
  (deactivate-mark))

(require 'sarcasm-utils)		;utility functions
(require 'sarcasm-el-get)               ;el-get packages and config
(require 'sarcasm-keys)			;global keybindings
(require 'sarcasm-rcirc)		;rcirc settings
(require 'sarcasm-org)			;Org-Mode settings
(require 'sarcasm-org-latex)            ;Org-Mode LaTex export config
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
(require 'sarcasm-comment)              ;Comments settings
(require 'sarcasm-netsoul)              ;NetSoul stuff
(require 'sarcasm-uniquify)             ;Uniquify buffer names
(require 'sarcasm-mode-line)            ;Mode-Line content
(require 'sarcasm-gud)                  ;Debugger settings

 ;; Gnus starting file (another way to say "(require 'sarcasm-gnus)")
(setq gnus-init-file (concat *sarcasm-directory*
                             "sarcasm-gnus")) ;it will try the suffix .el[c]

;; Custom settings
(setq custom-file (concat *sarcasm-directory*
                          "sarcasm-custom.el"))
(load custom-file)

;; Color theme
(setq custom-theme-directory *sarcasm-directory*)
(load-theme 'sarcasm)
