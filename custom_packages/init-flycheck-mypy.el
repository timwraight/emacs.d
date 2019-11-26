;; So far, I've been only been getting mypy to return *anything* with '--ignore-missing-imports'
(setq flycheck-python-mypy-args ' (
                                  "--follow-imports=skip" 
                                  "--ignore-missing-imports"
                                  "--allow-untyped-globals"
                                  ))


(flycheck-add-next-checker 'python-flake8 'python-mypy)
