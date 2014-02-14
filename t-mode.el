;; T-KEYS

;; A minor-mode for some of my customisations. This means I can put all my
;; custom keybindings in here and they will override the major mode bindings
;; each time without me having to redefine them each time.

(defvar tkeys-mode-map (make-keymap) "tkeys-mode keymap.")

;; MOVING BETWEEN WINDOWS
(global-set-key (kbd "C-<left>") 'windmove-left)
(global-set-key (kbd "C-<right>") 'windmove-right)
(global-set-key (kbd "C-<up>") 'windmove-up)
(global-set-key (kbd "C-<down>") 'windmove-down)

;; PREVIOUS/NEXT BUFFER
(global-set-key (kbd "C-S-<up>") 'previous-buffer)
(global-set-key (kbd "C-S-<down>") 'next-buffer)


(key-chord-define-global "wl" 'windmove-right)
(key-chord-define-global "wg" 'windmove-left)
(key-chord-define-global "wj" 'vimp-scroll-page-down)
(key-chord-define-global "wk" 'vimp-scroll-page-up)

(key-chord-define ac-menu-map "  " 'ac-stop)
(key-chord-define minibuffer-local-isearch-map "  " 'quit)

; KEYBINDINGS
(defun tim/jump-backward ()
    (interactive)
    (vimp-jump-backward)
    (recenter))

(defun tim/jump-forward ()
    (interactive)
    (vimp-jump-forward)
    (recenter))

(vimp-global-set-key 'normal " " 'tim/jump-backward)
(vimp-global-set-key 'normal "ซ" 'tim/jump-forward)

(vimp-global-set-key 'normal (kbd "<RET>") 'previous-buffer)
(vimp-global-set-key 'normal "ร" 'next-buffer)

(define-key vimp-normal-state-map (kbd "C-SPC") 'scroll-down)
(define-key vimp-insert-state-map "ซ" (kbd "<SPC>"))


(define-minor-mode tkeys-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'tkeys-mode-map)

(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'tkeys-mode))
      (let ((tkeys (assq 'tkeys-mode minor-mode-map-alist)))
        (assq-delete-all 'tkeys-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist tkeys))))
(ad-activate 'load)


(tkeys-mode 1)
