(defhydra sarcasm-hydra-bookmark (:color pink :hint nil)
  "
^Mark^             ^Unmark^           ^Actions^
^^^^^^^^--------------------------------------------
_d_: delete        _u_: unmark        _x_: execute
_D_: delete up     ^ ^                _r_: rename
^ ^                ^ ^                _R_: relocate
^ ^                ^ ^                _s_: save
"
  ("d" bookmark-bmenu-delete)
  ("D" bookmark-bmenu-delete-backwards)

  ("u" bookmark-bmenu-unmark)

  ("x" bookmark-bmenu-execute-deletions)
  ("r" bookmark-bmenu-rename)
  ("R" bookmark-bmenu-relocate)
  ("s" bookmark-bmenu-save)

  ("q" quit-window "quit" :color blue))

(defvar bookmark-bmenu-mode-map)
(defun sarcasm-hydray-bookmark-on ()
  (define-key bookmark-bmenu-mode-map "." 'sarcasm-hydra-bookmark/body))

(add-hook 'bookmark-bmenu-mode-hook 'sarcasm-hydray-bookmark-on)
