(use-package eglot
  :ensure t
  :hook (c++-mode . eglot-ensure)
  :init
  (setq eglot-server-programs '(((c++-mode c-mode) . ("clangd" "-clang-tidy")))))

(use-package flymake
  :defer t
  :bind ("C-c ! l" . flymake-show-diagnostics-buffer)
  :init
  ;; Configure `display-buffer' behaviour for some special buffers.
  ;; see http://www.lunaryorn.com/2015/04/29/the-power-of-display-buffer-alist.html
  ;; and https://github.com/lunaryorn/.emacs.d/blob/2233f7dc277453b7eaeb447b00d8cb8d72435318/init.el#L420-L439
  (add-to-list 'display-buffer-alist
               '("\\`\\*Flymake diagnostics"
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.22))))
