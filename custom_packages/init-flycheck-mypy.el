(flycheck-add-next-checker 'python-flake8 'python-mypy)
(setq flycheck-python-mypy-args '("--follow-imports=silent"  ; On CI, this should be 'silent' probably
                                  "--strict-optional"
                                  "--incremental"
                                  "--ignore-missing-imports"))
