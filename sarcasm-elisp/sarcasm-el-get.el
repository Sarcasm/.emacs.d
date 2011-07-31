;; el-get packages and config -- Guillaume Papin
;; usage:
;; (require 'sarcasm-el-get)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/el-get/el-get/"))
(require 'el-get)

(add-to-list 'el-get-recipe-path
             (concat (file-name-as-directory *sarcasm-load-path*)
                     "sarcasm-recipes"))

(setq el-get-sources
      '(el-get
        xcscope xcscope+    ;CScope stuff
        rainbow-mode        ;display string color colored
        ;; lua-mode            ;Lua-Mode in Emacs 24 is too old
        flymake-lua         ;flymake for Lua
        htmlize             ;for Org-Mode HTML export of source code
        folding             ;folding plugin
        rinari              ;Rinari Is Not A Ruby IDE
        haml-mode           ;Alternative to ERB
        sass-mode           ;Alternative to CSS
        yaml-mode           ;YAML Ain't Markup Language
        flymake-ruby        ;flymake for ruby
        magit               ;control git from Emacs
        fringe-helper       ;useful with test-case-mode
        dired-details       ;allow to only show filenames in dired buffer

        (:name emacschrome
               :features edit-server
               :after (lambda ()
                        (edit-server-start)))

        (:name ace-jump-mode       ;a quick cursor jump mode for Emacs
               :features ace-jump-mode
               :after (lambda ()
                        (setq ace-jumpace-jump-mode-case-sensitive-search nil)
                        ;; ;; I never used `zap-to-char', if I need it
                        ;; ;; M-x zap[TAB] should be enough
                        (global-set-key (kbd "M-z") 'ace-jump-mode)))

        (:name lua-mode
               :type git
               :url "https://github.com/immerrr/lua-mode.git"
               :features lua-mode
               :post-init (lambda ()
                            (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
                            (autoload 'lua-mode "lua-mode" "Lua editing mode." t)))

        (:name yari  ;Ri documentation in Emacs
               :features yari
               :after (lambda ()
                        (defun ri-bind-key ()
                          ;; (local-set-key (kbd "C-h v") 'yari-anything)
                          (local-set-key (kbd "C-c f") 'yari))
                        (add-hook 'ruby-mode-hook 'ri-bind-key)))

        (:name test-case-mode
               :after (lambda ()
                        (add-hook 'find-file-hook 'enable-test-case-mode-if-test)
                        (add-hook 'compilation-finish-functions 'test-case-compilation-finish-run-all)
                        (global-set-key [C-f11] (lambda ()
                                                  (interactive)
                                                  (test-case-run-all)
                                                  (test-case-echo-failure-mode t)
                                                  ))
                        (add-hook 'test-case-result-mode-hook 'test-case-echo-failure-mode)))

        (:name autopair
               :after (lambda ()
                        ;; (autopair-global-mode 1)
                        (add-hook 'find-file-hook 'autopair-mode)))

        (:name doxymacs
               ;; Use an alternative url (a fork with really minor
               ;; changes)
               :url "git://github.com/Sarcasm/doxymacs.git"
               :after (lambda ()
                        (setq doxymacs-command-character "\\")

                        (add-hook 'c-mode-common-hook 'doxymacs-mode)
                        (defun my-doxymacs-font-lock-hook ()
                          (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
                              (doxymacs-font-lock)))
                        (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)))

        (:name filladapt
               :features filladapt
               :after (lambda ()
                        ;; (setq-default filladapt-mode t)
                        (add-hook 'c-mode-common-hook 'c-setup-filladapt)
                        (add-hook 'c-mode-common-hook 'auto-fill-mode)
                        (add-hook 'c-mode-common-hook 'filladapt-mode)))

        ;; Ruby/HTML files
        (:name rhtml-mode
               :after (lambda ()
                        (rinari-launch)))

        ;; M-x with IDO
        (:name smex
               :after (lambda ()
                        (global-set-key (kbd "M-x") 'smex)
                        (global-set-key (kbd "M-X") 'smex-major-mode-commands)
                        ;; This is your old M-x.
                        (global-set-key (kbd "C-c M-x") 'execute-extended-command)))

        (:name iedit
               :after (lambda ()
                        (global-set-key (kbd "C-;") 'iedit-mode)
                        (define-key isearch-mode-map (kbd "C-;") 'iedit-mode)
                        (setq iedit-occurrence-face isearch-face)))

        (:name zencoding-mode
               ;; https://github.com/rooney/zencoding
               ;; http://www.emacswiki.org/emacs/ZenCoding
               :after (lambda ()
                        (add-hook 'sgml-mode-hook 'zencoding-mode))) ;auto-start on any markup modes

        (:name offlineimap              ;OfflineIMAP inside Emacs
               :after (add-hook 'gnus-before-startup-hook 'offlineimap))

        ;; Move buffer with C-S-<arrow key>
        (:name buffer-move
               :features buffer-move
               :after (lambda ()
                        (global-set-key (kbd "<C-S-up>")     'buf-move-up)
                        (global-set-key (kbd "<C-S-down>")   'buf-move-down)
                        (global-set-key (kbd "<C-S-left>")   'buf-move-left)
                        (global-set-key (kbd "<C-S-right>")  'buf-move-right)))

        (:name switch-window
               ;; re-define `C-x o' to `switch-window' because
               ;; it doesn't work the first time...
               :after (lambda ()
                        (global-set-key (kbd "C-x o") 'switch-window)))

        (:name yasnippet
               :type svn
               :url "http://yasnippet.googlecode.com/svn/trunk/"
               ;; El-get default rule compile *.el, but with
               ;; yasnippet-debug.el it failed. In the Rakefile the
               ;; correct task seems to be 'rake compile'
               :build ("rake compile")
               :features yasnippet
               :after (lambda ()
                        ;; from here: https://github.com/blastura/dot-emacs/blob/master/init.el
                        ;; (add-hook 'yas/after-exit-snippet-hook
                        ;;           '(lambda ()
                        ;;              (indent-region yas/snippet-beg
                        ;;                             yas/snippet-end)))

                        ;; After el-get/yasnippet, personal snippets takes priority
                        (setq yas/snippet-dirs (cons (concat el-get-dir "yasnippet/snippets")
                                                     '("~/.emacs.d/snippets")))

                        ;; Map `yas/load-directory' to every element
                        (mapc 'yas/load-directory yas/snippet-dirs)

                        ;; Fix the promp on X, the default was ugly.
                        (require 'dropdown-list)
                        (setq yas/prompt-functions '(yas/ido-prompt
                                                     yas/dropdown-prompt
                                                     yas/completing-prompt
                                                     yas/no-prompt))))

        (:name autocomplete
               :type git
               :url "http://github.com/m2ym/auto-complete.git"
               :load-path "."
               :post-init (lambda ()
                            (require 'auto-complete)
                            (add-to-list 'ac-dictionary-directories (expand-file-name "dict" pdir))
                            (require 'auto-complete-config)
                            (ac-config-default)
                            ;; Too many words in buffers...
                            ;; (setq-default ac-sources
                            ;;               (remq 'ac-source-words-in-same-mode-buffers ac-sources)

                            ;; Enable auto-completion with tab in Org-Mode
                            ;; http://permalink.gmane.org/gmane.emacs.orgmode/37064
                            (define-key ac-complete-mode-map [tab] 'ac-expand)))

        (:name auto-complete-extension
               :type emacswiki)

        (:name auto-complete-clang
               :type git
               :url "https://github.com/brianjcj/auto-complete-clang.git"
               :features auto-complete-clang)

        (:name ac-slime                 ;auto-complete for SLIME
               :post-init (lambda ()
                            (require 'ac-slime)
                            (add-hook 'slime-mode-hook 'set-up-slime-ac)
                            (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
                            (add-hook 'slime-connected-hook
                                      '(lambda ()
                                         ;; replace `yas/expand' by `auto-complete' ?
                                         ;; (define-key slime-mode-map (kbd "TAB") 'yas/expand)
                                         (define-key slime-repl-mode-map (kbd "TAB") 'yas/expand)))))))

;; Initialize el-get packages
(el-get)

;; "Addons" to the YASnippet config and auto-complete

;; Function found here: http://www.emacswiki.org/emacs/tagging.el
(defun convert-mode-name-to-hook (mode-name)
  "Converts a mode name into the symbol for its hook"
  (intern (concat (symbol-name mode-name) "-hook")))

;; Enable yasnippet mode and auto-complete on few programming modes
(defun sarcasm-enable-ac-and-yas ()
  "Enable `auto-complete' and `yasnippet'. Also add snippet names
in auto-complete sources."
  (yas/minor-mode-on)
  (auto-complete-mode)
  (setq ac-sources (append ac-sources '(ac-source-yasnippet)))
  ;; This is certainly not the good place for that...but for the
  ;; moment it's ok
  (setq show-trailing-whitespace t))

(mapc (lambda (mode)
        (add-hook (convert-mode-name-to-hook mode) 'sarcasm-enable-ac-and-yas))
      '(c-mode c++-mode emacs-lisp-mode lisp-mode lua-mode
               sh-mode perl-mode css-mode html-mode nxml-mode
               python-mode ruby-mode snippet-mode slime-mode
               slime-repl-mode))

(provide 'sarcasm-el-get)
