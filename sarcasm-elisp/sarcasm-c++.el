;; C++ stuff -- Guillaume Papin
;; usage:
;; (require 'sarcasm-c++)

(define-abbrev-table 'c++-mode-abbrev-table
  '(
    ("ALGO"		"#include <algorithm>")
    ("ASSERT"		"#include <cassert>")
    ("BIT"		"#include <bitset>")
    ("CSTD"		"#include <cstdlib>")
    ("CMATH"		"#include <cmath>")
    ("DEQUE"		"#include <deque>")
    ("IO"		"#include <iostream>")
    ("EXCEPTION"	"#include <exception>")
    ("LIST"		"#include <list>")
    ("MAP"		"#include <map>")
    ("QUEUE"		"#include <queue>")
    ("SET"		"#include <set>")
    ("STACK"		"#include <stack>")
    ("STD"		"#include <cstdlib>")
    ("STR"		"#include <string>")
    ("UNI"		"#include <unistd.h>")
    ("VECTOR"		"#include <vector>")
    ))

(add-hook 'c++-mode-hook
          (lambda ()
            (subword-mode)              ;C-c C-w to toggle
            (define-key c++-mode-map (kbd "M-TAB") 'ac-complete-clang)

            ;; (setq ac-sources (append '(ac-source-clang) ac-sources))
          ))

(provide 'sarcasm-c++)
