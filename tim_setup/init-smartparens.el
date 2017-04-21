(require 'smartparens-config)
(require 'smartparens-python)
(require 'smartparens-haskell)

(add-hook 'inferior-idris 'sp--haskell-mode-hooksetup-inferior-haskell-mode-search-bounds)
(smartparens-global-mode)
