(autoload 'erc-nick-notify-mode "erc-nick-notify"
  "Minor mode that calls `erc-nick-notify-cmd' when his nick gets
mentioned in an erc channel" t)
(setq erc-nick-notify-cmd "terminal-notifier -message ")
(eval-after-load 'erc '(erc-nick-notify-mode t))
