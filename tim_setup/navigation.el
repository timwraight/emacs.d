                                        ; NAVIGATION

(require 'dash)

;; RECENT FILES
(recentf-mode t)
(setq recentf-max-saved-items 200)
(add-to-list 'recentf-exclude "\\TAGS\\'")


(defun last-file-buffer ()
  (car (-filter (lambda (buf) (buffer-file-name buf)) (buffer-list)))
  )

(defun last-file ()
  (let
      ((file (nth 0 (or recentf--files-with-key recentf-list))))
    (find-file-noselect file)
  ))

(defun last-buffer-or-file ()
  (let
      ((last-buffer (or (last-file-buffer) (last-file) (get-buffer "*sratch*"))))
    (print last-buffer)
    last-buffer
    )
  )

;; Uniqify allows us to tell buffers with similar names apart
(setq uniquify-buffer-name-style 'forward)
(require 'uniquify)

(global-set-key (kbd "<f6>") 'direx-project:jump-to-project-root)
(global-set-key (kbd "<f7>") 'helm-bookmarks)
(global-set-key (kbd "C-x q") (lambda () (interactive) (save-buffers-kill-emacs t)))


(defun other-window-kill-buffer ()
  "Kill the buffer in the other window"
  (interactive)
  ;; Window selection is used because point goes to a different window
  ;; if more than 2 windows are present
  (let ((win-curr (selected-window))
        (win-other (next-window)))
    (select-window win-other)
    (kill-this-buffer)
    (select-window win-curr)))

;; IDO MODE
(eval-after-load "ido"
  '(progn
     (setq ido-enable-prefix nil
           ido-enable-flex-matching t
           ido-use-virtual-buffers t
           ido-create-new-buffer 'always
           ido-use-filename-at-point nil
           ido-max-prospects 10)
     (add-to-list 'ido-ignore-files "\\`pyc")
     (setq ido-everywhere t)
     (ido-mode 1)))

(define-key ido-buffer-completion-map (kbd "<escape>") 'abort-recursive-edit)

;; SCROLLING

(setq scroll-margin 3
      scroll-step  1
      scroll-conservatively 10000)


;; GOOGLE
(defun google-this () (interactive)
  (browse-url
   (let ((search-term
          (cond ((region-active-p)
                 (buffer-substring-no-properties (region-beginning) (region-end)))
                ((not (eq (thing-at-point 'symbol) nil))
                 (thing-at-point 'symbol))
                ((read-string "Search for term: ")))))
     (concat "http://www.google.co.uk/search?q=" search-term "&ie=utf-8"))))


; marks
(setq set-mark-command-repeat-pop t)
(setq global-mark-ring-max 100)
