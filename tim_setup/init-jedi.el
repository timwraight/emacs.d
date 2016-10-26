(setq jedi:environment-root "jedi")  ; or any other name you like
(setq py-python-command "/usr/local/bin/python3")
(add-hook 'python-mode-hook 'jedi:setup)
;; (add-hook 'jedi-mode-hook 'jedi-direx:setup)
(setq jedi:complete-on-dot t)
(setq jedi:setup-keys t)
(setq jedi:tooltip-method nil)

; Note that the second param here controls whether jedi jumps to the definition
; of the function or its assignment. nil means go to assignment. And the first
; arg is whether to open in new window or not.
(setq jedi:goto-definition-config '((nil definition nil)     ; C-.
                                    (t definition nil)          ; C-u C-.
                                    (t nil nil)            ; C-u C-u C-.
                                    (t definition nil)
                                    (nil nil t)
                                    (t nil t)
                                    (nil definition t)
                                    (t definition t)))
(eval-after-load "company-mode"
  (add-to-list 'company-backends 'company-jedi)) 

