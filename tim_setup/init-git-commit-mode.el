(defun git-kill-to-comments ()
  "Kill down from the cursor to the '# Please enter the commit message' bit"
  (interactive)
  (setq previous-point (point))
  (search-forward "\n# Please enter")
  (setq match-beginning-pos (match-beginning 0))
  (kill-region previous-point (- match-beginning-pos 1))
  (goto-char previous-point)
  )
