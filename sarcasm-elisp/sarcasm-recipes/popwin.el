(:name popwin
       :website "https://github.com/m2ym/popwin-el"
       :description "Popup Window Manager for Emacs."
       :type git
       :url "git://github.com/m2ym/popwin-el.git"
       :load-path ("." "./misc")
       :features popwin
       :post-init (lambda ()
                    (setq display-buffer-function 'popwin:display-buffer)))
