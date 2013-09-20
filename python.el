;; HOOKS

(add-hook 'python-mode-hook 
      (lambda ()
        (setq ac-sources '(ac-source-symbols ac-source-words-in-same-mode-buffers))))


(add-hook 'python-mode-hook
  (lambda()
    (add-hook 'write-contents-functions
      (lambda()
        (save-excursion
          (delete-trailing-whitespace))))))


