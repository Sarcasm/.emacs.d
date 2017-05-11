(defadvice narrow-to-region (after narrow-to-region-unmark activate compile)
  "Disable selection after `narrow-to-region'."
  (deactivate-mark))
