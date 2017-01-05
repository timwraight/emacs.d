(flycheck-add-next-checker 'python-flake8 'python-mypy)
(setq flycheck-python-mypy-args '("--silent-imports"
                                  "--strict=optional"
                                  "--fast-parser"))
