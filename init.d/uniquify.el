(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward ; name|foo/bar
        uniquify-strip-common-suffix t
        uniquify-separator " :: "
        ;; rename after killing uniquified
        uniquify-after-kill-buffer-p t
        ;; don't muck with special buffers
        ;; uniquify-ignore-buffers-re "^\\*"
        ))
