;; Used while coding an HTTP server (school project)
;; RFC-LINK-DIRECTORY should be replaced

;; Follow links that looks like \rfc{section} or \rfclink{section}
;; when working in C++ (Zia project).
(add-to-list 'load-path RFC-LINK-DIRECTORY)
(require 'rfclink)
(add-hook 'c++-mode-hook 'rfclink-mode)
