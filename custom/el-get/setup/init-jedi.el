(setq jedi:server-args
      '("--virtual-env" "~/Envs/mypc"))
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

