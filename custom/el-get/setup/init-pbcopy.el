(require 'pbcopy)
(turn-on-pbcopy)
; disable for tramp
(add-hook 'tramp-mode 'turn-off-pbcopy)
