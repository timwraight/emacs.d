(require 'projectile)
(projectile-mode)
(setq projectile-remember-window-configs t)
(setq projectile-enable-caching nil)
(setq projectile-completion-system 'helm)
(global-set-key "\C-c p p" 'projectile-switch-project)
(setq projectile-switch-project-action 'projectile-find-file)
(define-key helm-projectile-projects-map (kbd "M-e") 'helm-next-line)

