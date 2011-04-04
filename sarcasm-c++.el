;; C++ stuff -- Guillaume Papin
;; usage:
;; (require 'sarcasm-c++)

(define-abbrev-table 'c++-mode-abbrev-table
  '(
    ("ALGO"		"#include <algorithm>")
    ("ASSERT"		"#include <cassert>")
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
            (setq ac-sources (append '(ac-source-clang) ac-sources)))
          )

(provide 'sarcasm-c++)
