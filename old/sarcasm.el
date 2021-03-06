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
;; (when (file-exists-p "/mnt/media/projects/")
;;   (setq default-directory "/mnt/media/projects/"))

;; I don't understand why, but the following doesn't work
;; ;; Change the font to your needs
(set-default-font "DejaVu Sans Mono-10")
(set-frame-font "DejaVu Sans Mono-10")
(setq default-frame-alist '((font . "DejaVu Sans Mono-10"))) ;for emacs --daemon
;; So in ~/.Xdefaults / ~/.Xresources
;;
;; Emacs.FontBackend: xft
;; Emacs.font: DejaVu Sans Mono-10

(setq inhibit-startup-screen t       ;do not display a startup screen
      autopair-autowrap t            ;wrap the region with the paired characters
      mouse-yank-at-point t          ;paste at cursor position
      scroll-preserve-screen-position t ;restore cursor after PgUp/PgDown

      ;; Sentences end with one space when M-q `fill-paragraph' is called
      sentence-end-double-space nil

      compilation-auto-jump-to-first-error t
      compilation-scroll-output t

      ;; C-k kills whole line and newline if at beginning of line
      kill-whole-line t

      save-abbrevs 'silently            ;don't want to answer yes everytime

      ;; Autosave each change)
      bookmark-save-flag 1

      ;; Use Window system clipboard
      x-select-enable-clipboard t
      yank-pop-change-selection t

      ;; Case-insensitive `find-file'
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t

      ;; "Smooth" mouse scrolling, one line at a time
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      )

(setq-default indent-tabs-mode nil)     ;remove tabulations

;; Emacs external `url browser' (usefull in Org-Mode)
;; TODO: Use the customize interface for this, it's different every damn time
(setq-default browse-url-generic-program "chromium")
(setq browse-url-generic-program "chromium")
(setq browse-url-browser-function '(("^file:" . browse-url-generic)
				    ("."      . browse-url-generic)))

;; http://www.emacswiki.org/emacs/SavePlace
;; note: This is useful when =C-x C-v= is done
(setq-default save-place t)
(require 'saveplace)
(setq save-place-file (concat user-emacs-directory "places"))

(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
;; When using C-x C-e to edit the command line
(add-to-list 'auto-mode-alist '("\\`/tmp/zshecl[0-9]+\\'" . sh-mode))

;; Qt stuff
(add-to-list 'auto-mode-alist '("\\.pro\\'" . makefile-mode))

;; Thanks http://www.emacswiki.org/emacs/JonathanArnoldDotEmacs
;; note: end with a slash for directories
(setq completion-ignored-extensions '(".o" ".a" ".so"
				      ".elc"
                                      ".pyc" "__pycache__"
                                      ".class" ".dll"
				      "~"
				      ".git/"
				      ".gitignore"
				      ".hg/"
				      ".hgignore"
				      ".svn/"))

(defvar sarcasm-ignored-files '("GPATH" "GRTAGS" "GTAGS"
                                ".git" ".gitignore"
                                ".hg" ".hgignore"
                                "__pycache__"
                                ;; core.clj was ignored in `dired-omit-files'
                                ;; "core"
                                "vgcore"
                                ".newsrc-dribble")
  "A list of filename and directory to ignore (no directory
separator should be involved).

note: at this time this variable is used for making the
`dired-omit-files' and some `eproject' project type.")

(defvar sarcasm-ignored-files-re '("cscope\\.\\w+"
                                   ;; coredump and valgrind coredump
                                   "\\(?:\\vg\\)?core\\.[[:digit:]]+")
  "See `sarcasm-ignored-files'.

There only difference is that each filename should be a regexp.")

;; This sets the coding system priority and the default input method
;; and sometimes other things.
(set-language-environment "UTF-8")

(setenv "EDITOR" "emacsclient")

;; (menu-bar-mode -1)
(tool-bar-mode -1) ;toolbar is visible by default on X emacs so disable it
(blink-cursor-mode -1)                  ;it's annoying
(scroll-bar-mode -1)
(column-number-mode 1)                  ;print column number on the mode-line
(winner-mode 1)

;; useful to get 'compile-command' between session without having to retype it
;; everytime
(savehist-mode 1)

;; see http://dfan.org/blog/2009/02/19/emacs-dedicated-windows/
(defun toggle-current-window-dedication ()
  (interactive)
  (let* ((window (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

(global-set-key [pause] 'toggle-current-window-dedication)

;; Emacs has a slightly smaller value by default but sometimes it cuts
;; too short.
(setq-default fill-column 80)

(defun sarcasm-show-columm-80 ()
  (setq-local whitespace-style '(face tabs trailing lines-tail))
  (setq-local whitespace-line-column 80)
  (whitespace-mode 1))
(add-hook 'prog-mode-hook 'sarcasm-show-columm-80)
(add-hook 'rst-mode-hook 'sarcasm-show-columm-80)

(defun sarcasm-no-whitespace-mode ()
  (whitespace-mode -1))
(add-hook 'makefile-mode-hook 'sarcasm-no-whitespace-mode)

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

(defadvice narrow-to-region (after narrow-to-region-unmark
activate compile)
  "Disable selection after `narrow-to-region'"
  (deactivate-mark))

;; Load the identity file where mail address, user full name, etc, are
;; recommended to be set if the file exists.
;;
;; An example of the content of this file can be:
;;
;;    $ cat sarcasm-identity.el
;;    (setq user-mail-address "firstname.lastname@gmail.com"
;;          user-full-name    "Firstname Lastname")
;;    (provide 'sarcasm-identity)
;;    $
(require 'sarcasm-identity nil t)

(require 'sarcasm-utils)		;utility functions

;; ELPA should be used use at the beginning, so dependent package will
;; be configured correctly.
(require 'sarcasm-elpa)			;ELPA (Emacs Lisp Package Archive) stuff

;; (require 'sarcasm-el-get)               ;el-get packages and config
(require 'sarcasm-keys)			;global keybindings
(require 'sarcasm-rcirc)		;rcirc settings
(require 'sarcasm-c-common)		;C/C++ common stuff
(require 'sarcasm-c)			;C stuff
(require 'sarcasm-c++)			;C++ stuff
(require 'sarcasm-flymake)              ;flymake stuff
(require 'sarcasm-makefile)             ;Makefile stuff
(require 'sarcasm-lua)                  ;Lua stuff
(require 'sarcasm-ruby)			;Ruby stuff
(require 'sarcasm-python)               ;Python stuff
(require 'sarcasm-lisp)			;Lisp stuff
;; (require 'sarcasm-gtags)                ;GTags stuff
(require 'sarcasm-semantic)             ;General Semantic stuff
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
(require 'sarcasm-org)                  ;Org-Mode settings
;; (require 'sarcasm-org-latex)            ;Org-Mode LaTex export config
(require 'sarcasm-pcmpl-ninja)          ;Ninja pcomplete integration
(require 'sarcasm-compilation)          ;Compilation stuff
(require 'sarcasm-js)                   ;Javascript stuff
(require 'sarcasm-serial-term)          ;Serial terminal specifics

;; Custom settings
(setq custom-file (concat *sarcasm-directory*
                          "sarcasm-custom.el"))
(load custom-file)

(require 'sarcasm-irony)                ;Irony-Mode development stuff

 ;; Gnus starting file (another way to say "(require 'sarcasm-gnus)")
(setq gnus-init-file (concat *sarcasm-directory*
                             "sarcasm-gnus")) ;it will try the suffix .el[c]

;; Color theme
(setq custom-theme-directory *sarcasm-directory*)
(load-theme 'sarcasm t)

;; Replace selection
(delete-selection-mode 1)
