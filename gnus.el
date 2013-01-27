; Also check .gnus. 
(require 'gnus-load)
(add-hook 'gnus-article-mode-hook (lambda () (variable-pitch-mode t)))

(shell-command "offlineimap&" "*offlineimap*" nil)

