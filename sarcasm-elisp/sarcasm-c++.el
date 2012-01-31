;; C++ stuff -- Guillaume Papin
;; usage:
;; (require 'sarcasm-c++)

(define-abbrev-table 'c++-mode-abbrev-table
  '(
    ("ALGO"		"#include <algorithm>")
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
    ("VECTOR"		"#include <vector>")
    ))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(add-hook 'c++-mode-hook
          '(lambda ()
             (subword-mode)             ;C-c C-w to toggle

             ;; No additional indentation for members of a namespace.
             ;; (c-set-style "stroustrup")
             (c-set-offset 'innamespace 0)

             (define-key c++-mode-map (kbd "M-TAB") 'ac-complete-clang)))

(provide 'sarcasm-c++)
