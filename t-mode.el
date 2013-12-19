;; T-KEYS

;; A minor-mode for some of my customisations. This means I can put all my
;; custom keybindings in here and they will override the major mode bindings
;; each time without me having to redefine them each time.

(defvar tkeys-mode-map (make-keymap) "tkeys-mode keymap.")

(define-key tkeys-mode-map (kbd "C-S-<down>") 'previous-buffer)
(define-key tkeys-mode-map (kbd "C-S-<up>") 'next-buffer)


(define-minor-mode tkeys-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'tkeys-mode-map)

(tkeys-mode 1)
