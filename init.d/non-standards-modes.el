(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package yaml-mode
  :ensure t
  ;; FIXME: is there a clean way to put this in clang-format config instead?
  :mode ("\\.clang-format\\'" . yaml-mode)
  :mode ("\\.clang-tidy\\'" . yaml-mode))

(use-package markdown-mode
  :ensure t
  :defer t
  :config
  ;; see https://github.com/jackrusher/dotemacs/issues/27
  (set-face-attribute 'markdown-code-face nil :background nil))

(use-package cmake-mode
  :ensure t
  :defer t)

(use-package qml-mode
  :ensure t
  :defer t)
