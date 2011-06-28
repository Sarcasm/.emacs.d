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

(add-hook 'c++-mode-hook
          (lambda ()
            (subword-mode)              ;C-c C-w to toggle
            (define-key c++-mode-map (kbd "M-TAB") 'ac-complete-clang)

            (define-key c++-mode-map (kbd "C-c t") 'c++-open-decl-or-def-other-window)
            ;; (setq ac-sources (append '(ac-source-clang) ac-sources))
          ))

(defun c++-open-decl-or-def-other-window ()
  "Open the declaration (Class.hh) or the definition (Class.cpp)
of the current file in other window.

Create the file if it doesn't exist.

File.hh => File.cpp
File.cpp => File.hh"
  (interactive)
  (let ((file buffer-file-name))
    (if (not file)
        (message "Coulnd't retrieve buffer filename.")
      (let ((ext (file-name-extension file)))
        (cond
         ((string= ext "hh")
          (find-file-other-window (concat (file-name-sans-extension file) ".cpp")))
         ((string= ext "cpp")
          (find-file-other-window (concat (file-name-sans-extension file) ".hh")))
         (t (message "Invalid filename, extension .hh or .cpp expected."))
         )
        )
      )
    )
  )

(provide 'sarcasm-c++)
