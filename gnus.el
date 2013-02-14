; Also check .gnus. 
(require 'gnus-load)
(add-hook 'gnus-article-mode-hook (lambda () (variable-pitch-mode t)))
(setq gnus-asynchronous t)

(setq gnus-thread-sort-functions
           '(gnus-thread-sort-by-number
             gnus-thread-sort-by-most-recent-date))

(require 'google-contacts)
(require 'google-contacts-gnus)
(require 'google-contacts-message)

(gnus-demon-add-handler 'gnus-demon-scan-news 2 t) 
