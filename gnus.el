; Also check .gnus. 
(require 'gnus-load)
(add-hook 'gnus-article-mode-hook (lambda () (variable-pitch-mode t)))

(setq gnus-thread-sort-functions
           '(gnus-thread-sort-by-number
             gnus-thread-sort-by-most-recent-date))
