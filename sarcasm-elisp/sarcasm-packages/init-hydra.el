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
(defun sarcasm-hydra-bookmark-on ()
  (define-key bookmark-bmenu-mode-map "." 'sarcasm-hydra-bookmark/body))

(add-hook 'bookmark-bmenu-mode-hook 'sarcasm-hydra-bookmark-on)

(defhydra sarcasm-hydra-projectile (:color blue :hint nil :idle 0.4)
  "
  Files^^             Search^^          Buffer^^         Other Window^^   Run^^           Cache^^
╭────────────────────────────────────────────────────────────────────────────────────────────────────╯
  [_f_] file          [_a_] ag          [_b_] switch     [_F_] file       [_U_] test      [_kc_] clear
  [_l_] file dwim     [_A_] grep        [_v_] show all   [_L_] dwim       [_m_] compile   [_kk_] add current
  [_r_] recent file   [_s_] occur       [_V_] ibuffer    [_D_] dir        [_c_] shell     [_ks_] cleanup
  [_d_] dir           [_S_] replace     [_K_] kill all   [_O_] other      [_C_] command   [_kd_] remove
  [_o_] other         [_t_] find tag     ^ ^             [_B_] buffer
  [_u_] test file     [_T_] make tags
  [_h_] root
"
  ("<tab>" hydra-master/body "back")
  ("<ESC>" nil "quit")
  ("a"   projectile-ag)
  ("A"   projectile-grep)
  ("b"   projectile-switch-to-buffer)
  ("B"   projectile-switch-to-buffer-other-window)
  ("c"   projectile-run-async-shell-command-in-root)
  ("C"   projectile-run-command-in-root)
  ("d"   projectile-find-dir)
  ("D"   projectile-find-dir-other-window)
  ("f"   projectile-find-file)
  ("F"   projectile-find-file-other-window)
  ("h"   projectile-dired)
  ("i"   projectile-project-info)
  ("kc"  projectile-invalidate-cache)
  ("kd"  projectile-remove-known-project)
  ("kk"  projectile-cache-current-file)
  ("K"   projectile-kill-buffers)
  ("ks"  projectile-cleanup-known-projects)
  ("l"   projectile-find-file-dwim)
  ("L"   projectile-find-file-dwim-other-window)
  ("m"   projectile-compile-project)
  ("o"   projectile-find-other-file)
  ("O"   projectile-find-other-file-other-window)
  ("p"   projectile-commander)
  ("r"   projectile-recentf)
  ("s"   projectile-multi-occur)
  ("S"   projectile-replace)
  ("t"   projectile-find-tag)
  ("T"   projectile-regenerate-tags)
  ("u"   projectile-find-test-file)
  ("U"   projectile-test-project)
  ("v"   projectile-display-buffer)
  ("V"   projectile-ibuffer))

(defun sarcasm-hydra-projectile-on ()
  (global-set-key (kbd "C-c C-p") 'sarcasm-hydra-projectile/body))

(eval-after-load 'projectile
  (add-hook 'projectile-mode-hook 'sarcasm-hydra-projectile-on))
