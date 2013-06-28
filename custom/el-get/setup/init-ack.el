(vimp-leader/set-key "a" (lambda () (interactive)
                           (ack (concat "ack " (read-string "Search for: " (word-at-point)))
                                (helm-ls-git-root-dir))))
