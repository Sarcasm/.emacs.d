(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode)) ;CUDA
;; QMake makefiles
(add-to-list 'auto-mode-alist '("\\.pro\\'" . makefile-mode))

(use-package find-file
  :defer t
  :init (setq-default ff-always-in-other-window t))

(use-package cc-mode
  :bind (:map c-mode-base-map
              ("C-c t" . ff-find-other-file))
  :preface
  (defun sarcasm-set-c++-cc-style ()
    "Personalized cc-style for c++ mode."
    (c-set-offset 'innamespace 0))
  :config
  (add-hook 'c++-mode-hook #'sarcasm-set-c++-cc-style)

  (define-abbrev-table 'c-mode-abbrev-table
    '(("ASSERT"	"#include <assert.h>")
      ("ERRNO"	"#include <errno.h>")
      ("IO"	"#include <stdio.h>")
      ("STD"	"#include <stdlib.h>")
      ("STR"	"#include <string.h>")
      ("UNI"	"#include <unistd.h>")))

  (define-abbrev-table 'c++-mode-abbrev-table
    '(("ALGO"		"#include <algorithm>")
      ("ASSERT"		"#include <cassert>")
      ("BIT"		"#include <bitset>")
      ("CSTD"		"#include <cstdlib>")
      ("CIO"		"#include <cstdio>")
      ("CMATH"		"#include <cmath>")
      ("DEQUE"		"#include <deque>")
      ("IO"		"#include <iostream>")
      ("EXCEPTION"	"#include <exception>")
      ("LIST"		"#include <list>")
      ("MAP"		"#include <map>")
      ("MANIP"		"#include <iomanip>")
      ("QUEUE"		"#include <queue>")
      ("SET"		"#include <set>")
      ("SSTR"		"#include <sstream>")
      ("STACK"		"#include <stack>")
      ("STDEXCEPT"	"#include <stdexcep>")
      ("STR"		"#include <string>")
      ("UNI"		"#include <unistd.h>")
      ("VECTOR"		"#include <vector>"))))

(use-package clang-format
  :ensure t
  :after cc-mode
  :defines c-mode-base-map
  :bind (:map c-mode-base-map ("C-S-f" . clang-format-region))
  :config
  (use-package sarcasm-clang-format
    :defer t
    :commands sarcasm-clang-format-set-c-style
    :init (add-hook 'c++-mode-hook 'sarcasm-clang-format-set-c-style)))

(setq-default irony-user-dir (cache "irony"))

(use-package irony
  ;; special configuration because this is a development package
  :load-path "~/dev/ws/irony/irony-mode/"
  ;; needs to declare irony-cdb for autoloads to work
  :config (use-package irony-cdb)
  :commands irony-install-server

  ;; standard irony configuration
  :bind (:map irony-mode-map
              ("C-c C-b" . irony-cdb-menu)
              ("C-c =" . irony-get-type))
  :after cc-mode
  :preface
  (defun sarcasm-irony-cdb-not-found (command &rest args)
    (when (eq command 'get-compile-options)
      (message "Irony: compile options not found!")
      nil))
  (defvar irony-server-w32-pipe-buffer-size)
  :init
  (setq-default irony-cdb-compilation-databases '(irony-cdb-clang-complete
                                                  irony-cdb-libclang
                                                  sarcasm-irony-cdb-not-found))
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

  ;; Windows performance tweaks
  ;;
  (when (boundp 'w32-pipe-read-delay)
    (setq w32-pipe-read-delay 0))
  ;; Set the buffer size to 64K on Windows (from the original 4K)
  (when (boundp 'w32-pipe-buffer-size)
    (setq irony-server-w32-pipe-buffer-size (* 64 1024)))

  (use-package company-irony
    :load-path "~/dev/ws/irony/company-irony/"
    :after company
    :config
    (setq company-irony-ignore-case 'smart)
    (add-to-list 'company-backends 'company-irony)
    (use-package company-c-headers
      :ensure t
      :functions irony--extract-user-search-paths company-c-headers
      :preface
      (defun company-c-headers-path-user-irony ()
        "Return the user include paths for the current buffer."
        (when irony-mode
          (irony--extract-user-search-paths irony--compile-options
                                            irony--working-directory)))
      :config
      (setq company-c-headers-path-user #'company-c-headers-path-user-irony)
      (add-to-list 'company-backends #'company-c-headers)))

  (use-package flycheck-irony
    :load-path "~/dev/ws/irony/flycheck-irony/"
    :config (add-hook 'irony-mode-hook 'flycheck-irony-setup))

  ;; (use-package flycheck-clang-analyzer
  ;;   :load-path "~/dev/ws/irony/external/flycheck-clang-analyzer"
  ;;   :after flycheck-irony
  ;;   :config (add-hook 'irony-mode-hook 'flycheck-clang-analyzer-setup))
  )
