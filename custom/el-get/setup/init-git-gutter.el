(global-git-gutter-mode +1)
;; inactivate git-gutter-mode in asm-mode and image-mode
(custom-set-variables
 '(git-gutter:disabled-modes '(asm-mode image-mode)))

(custom-set-variables
 '(git-gutter:hide-gutter t))
