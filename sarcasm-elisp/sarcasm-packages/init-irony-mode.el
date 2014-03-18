;; The Clang installation missed the system include directory
;; "/usr/lib/clang/3.2/include/"
(when (file-exists-p "/usr/lib64/clang/3.2/include/")
  (setq irony-libclang-additional-flags
        '("-isystem" "/usr/lib64/clang/3.2/include/")))

;; Use Ninja (http://martine.github.io/ninja/) instead of classic Makefiles
(setq irony-cdb-cmake-generator "Ninja")

;; FIXME: Not elegant, find a better way to enable default plugins.
(autoload 'irony-enable "irony")
(irony-enable 'ac)

(defun sarcasm-enable-irony-mode ()
  ;; avoid enabling irony-mode in modes that inherits c-mode, e.g: php-mode
  (when (member major-mode irony-known-modes)
    ;; uncomment if other ac-sources are too annoying
    (setq ac-sources nil)

    ;; enable irony-mode
    (irony-mode 1)))

(add-hook 'c++-mode-hook 'sarcasm-enable-irony-mode)
(add-hook 'c-mode-hook 'sarcasm-enable-irony-mode)
