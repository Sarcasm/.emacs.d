(defvar company-backends)
(eval-after-load "company"
  '(progn
     (add-to-list 'company-backends 'company-anaconda)))
