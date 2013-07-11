(predictive-load-dict "english")
(setq predictive-auto-add-filter
      (lambda (word) (dictree-member-p dict-english word)))
(setq predictive-auto-complete nil)
