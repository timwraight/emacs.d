(require 'smartparens-config)
(require 'smartparens-python)
(require 'smartparens-haskell)

(add-hook 'inferior-idris 'sp--haskell-mode-hooksetup-inferior-haskell-mode-search-bounds)
(add-hook 'python-mode-hook #'smartparens-mode)
(add-hook 'scss-mode-hook #'smartparens-mode)
