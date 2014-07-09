(add-hook 'python-mode-hook 'jedi:setup)
;; (add-hook 'jedi-mode-hook 'jedi-direx:setup)
(setq jedi:complete-on-dot t)
(setq jedi:setup-keys t)
(setq jedi:goto-definition-config '((t nil nil)        ; C-.
                                    (nil nil nil)          ; C-u C-.
                                    (nil definition nil)
                                    (t definition nil)
                                    (nil nil t)
                                    (t nil t)
                                    (nil definition t)
                                    (t definition t)))
