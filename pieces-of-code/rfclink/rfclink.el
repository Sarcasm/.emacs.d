;;; rfclink.el --- Handle Doxygen comments about link to the HTTP rfc.

;; Copyright (C) 2012  Guillaume Papin

;; Author: Guillaume Papin <guillaume.papin@epitech.eu>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

(defgroup rfclink nil
  "Add enhancements to the C/C++ modes."
  :version "24.0"
  :group 'c)

(defvar rfclink-mode-line " \\rfc"
  "Mode line")
(make-variable-buffer-local 'rfclink-mode-line)

(define-minor-mode rfclink-mode
  "Toggle RFC Link mode.

With no argument, this command toggles the mode. Non-null prefix
argument turns on the mode. Null prefix argument turns off the
mode."
  nil
  rfclink-mode-line
  `((,(kbd "C-c g") . rfclink-refresh))
  (cond
   ;; fire up RFC Link !
   (rfclink-mode
    (rfclink-generate-overlays)
    (message "fire up !"))

   ;; Shutdown RFC Link...
   (t
    (rfclink-delete-overlays)
    (message "shutdown..."))
   ))

;; Tests

(defvar rfclink-overlays '()
  "List of RFC Link overlays.")

(defvar rfclink-re "\\\\rfc\\(?:link\\)?{\\(.+\\)}"
  "Size of words to overlay.")

(defun rfclink-delete-overlays ()
  "Supprime tous les overlays de `rfclink-overlays', set
`rfclink-overlays' à nil."
  (remove-overlays nil nil 'rfclink-overlay t))

(defvar rfclink-mode-overlay-keymap (make-keymap)
  "Keymap for overlay in RFC Link mode.")

;; Keymap similar to `goto-address'
(define-key rfclink-mode-overlay-keymap [(mouse-2)] 'rfclink-goto-current-section)
(define-key rfclink-mode-overlay-keymap (kbd "C-c RET") 'rfclink-goto-current-section)

(defun rfclink-generate-overlays ()
  "Génère les overlays de la taille `rfclink-word-size'."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      ;; note (elisp info node *Managing Overlays*):
      ;; A loop that scans the buffer forwards, creating overlays, can
      ;; run faster if you do `(overlay-recenter (point-max))' first.
      (overlay-recenter (point-max))
      (while (re-search-forward rfclink-re nil t)
        (let ((overlay (make-overlay (match-beginning 0)
                                     (match-end 0))))
          (overlay-put overlay 'rfclink-overlay t)
          ;; (overlay-put overlay 'category '(CATEGORIE_PROPERTIES))
          (overlay-put overlay 'face 'link)
          (overlay-put overlay 'mouse-face 'highlight)

          (overlay-put overlay 'rfclink-section (match-string 1))

          ;; (overlay-put overlay 'help-echo (format "This word has a size of %d." rfclink-word-size))
          ;; (overlay-put overlay 'before-string " > ")
          ;; (overlay-put overlay 'after-string " < ")
          ;; (overlay-put overlay 'invisible t)
          (overlay-put overlay 'local-map rfclink-mode-overlay-keymap))))))

(defun rfclink-refresh ()
  (interactive)
  (when rfclink-mode
    (rfclink-delete-overlays)
    (rfclink-generate-overlays)))

(defun rfclink-goto-section (section &optional other-window)
  (interactive)
  (let ((rfc-path (expand-file-name "~/projects/C++/zia/rfc2616.txt")))
    ;; Ugly fix
    (unless (string= buffer-file-name rfc-path)
      (if other-window
          (find-file-other-window (expand-file-name rfc-path))
        (find-file (expand-file-name rfc-path))))
    (with-current-buffer (find-buffer-visiting rfc-path)
      (goto-line 1)
      (search-forward (concat "\n" section " "))
      (beginning-of-line))))

(defun rfclink-goto-current-section (&optional other-window)
  (interactive "P")
  (dolist (ov (overlays-at (point)))
    (when (overlay-get ov 'rfclink-overlay)
      (rfclink-goto-section (overlay-get ov 'rfclink-section) other-window))))

(provide 'rfclink)
;;; rfclink.el ends here
