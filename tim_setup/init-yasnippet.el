(require 'yasnippet)
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/yasnippets")

(yas-global-mode 1)
(yas-reload-all)

;; This illustrates how to redefine yas-expand to S-TAB.
(define-key yas-minor-mode-map [backtab]     'yas-expand)
(define-key yas-minor-mode-map (kbd "M-e")     'yas-expand)


;; Strangely, just redefining one of the variations below won't work.
;; All rebinds seem to be needed.
(define-key yas-minor-mode-map [(tab)]        nil)
(define-key yas-minor-mode-map (kbd "TAB")    nil)
(define-key yas-minor-mode-map (kbd "<tab>")  nil)
