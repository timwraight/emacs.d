(require 'git-timemachine)
(global-set-key (kbd "C-c t") 'git-timemachine)
(define-key git-timemachine-mode-map (kbd "C-M-n") 'git-timemachine-show-previous-revision)
(define-key git-timemachine-mode-map (kbd "C-M-i") 'git-timemachine-show-next-revision)
(define-key git-timemachine-mode-map (kbd "C-M-q") 'git-timemachine-quit)
(setq git-timemachine-show-minibuffer-details t)
