(require 'ivy-overlay)
(require 'ivy-posframe)
;; (ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-display-style 'fancy)
(setq ivy-buffer-max-buffer-display-length 80)
;; (add-to-list 'ivy-display-functions-alist '(swiper . ivy-display-function-overlay))

(define-key ivy-minibuffer-map (kbd "M-e") 'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "M-u") 'ivy-previous-line)

(define-key counsel-find-file-map (kbd "M-n") 'counsel-up-directory)
(define-key counsel-find-file-map (kbd "M-i") 'counsel-down-directory)

(setq ivy-re-builders-alist
      '((t      . ivy--regex-ignore-order)))




(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))

(setq ivy-posframe-height-alist '((swiper . 20)
                                  (t      . 40)))
(setq ivy-posframe-parameters
      '((left-fringe . 12)
        (right-fringe . 12)))


(ivy-posframe-mode 1)

(defun ivy-display-function-window (text)
  (let ((buffer (get-buffer-create "*ivy-candidate-window*"))
        (str (with-current-buffer (get-buffer-create " *Minibuf-1*")
               (let ((point (point))
                     (string (concat (buffer-string) "  " text)))
                 (ivy-add-face-text-property
                  (- point 1) point 'ivy-cursor string t)
                 string))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert str)))
    (with-ivy-window
      (display-buffer
       buffer
       `((display-buffer-reuse-window
          (lambda (buf alist) (display-buffer-in-side-window buf '((side . right))))
          (window-height . ,(1+ (ivy--height (ivy-state-caller ivy-last))))
          (window-width . 0.5)
          ))))))

;; (setq ivy-display-functions-alist
;;       '((counsel-M-x . ivy-display-function-window)
;;         (counsel-git . ivy-display-function-window)
;;         (ivy-completion-in-region . ivy-display-function-window)))

(global-set-key "\M-x" 'counsel-M-x)
