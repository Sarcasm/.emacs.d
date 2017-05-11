(require 'bind-key)

(use-package avy
  :ensure t
  :bind ("C-:" . avy-goto-word-or-subword-1))

;; move buffers with C-S-<arrow key>
(use-package buffer-move
  :ensure t
  :bind (("<C-S-up>" . buf-move-up)
         ("<C-S-down>" . buf-move-down)
         ("<C-S-left>" . buf-move-left)
         ("<C-S-right>" . buf-move-right)))

(bind-keys ("<S-up>" . windmove-up)
           ("<S-down>" . windmove-down)
           ("<S-left>" . windmove-left)
           ("<S-right>" . windmove-right))

;; TODO: this needs a hydra for killing, maximising, ...
(use-package ace-window
  :ensure t
  :bind ([remap other-window] . ace-window)
  :init (setq aw-background nil))
