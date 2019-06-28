(setq avy-keys '(?a ?r ?s ?t ?n ?e ?i ?o))
(setq avy-all-windows nil)

(defun avy-goto-word-crt-line ()
  "Jump to a word start on the current line only."
  (interactive)
  (avy-with avy-goto-word-0
    (avy-goto-word-0 nil (line-beginning-position) (line-end-position))))

;; optional evil integration example
(declare-function 'avy-goto-word-crt-line "avy")
(with-eval-after-load "evil"
  (evil-define-avy-motion avy-goto-word-crt-line inclusive)
  )

