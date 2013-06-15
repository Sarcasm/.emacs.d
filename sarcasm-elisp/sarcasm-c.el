;; C stuff -- Guillaume Papin
;; usage:
;; (require 'sarcasm-c)

(define-abbrev-table 'c-mode-abbrev-table
  '(
    ("ASSERT"	"#include <assert.h>")
    ("ERRNO"	"#include <errno.h>")
    ("IO"	"#include <stdio.h>")
    ("STD"	"#include <stdlib.h>")
    ("STR"	"#include <string.h>")
    ("UNI"	"#include <unistd.h>")
    ))

(add-hook 'c-mode-hook
	  '(lambda ()
             ;; School requires that
             ;; (setq indent-tabs-mode t)
             ;; (define-key c-mode-map (kbd "M-TAB") 'ac-complete-clang)
             ))

(provide 'sarcasm-c)
