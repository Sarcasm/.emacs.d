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

(provide 'sarcasm-c)
