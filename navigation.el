; NAVIGATION

;; RECENT FILES
(recentf-mode t)
(setq recentf-max-saved-items 100)

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let* ((file-assoc-list
          (mapcar (lambda (x)
                    (cons (file-name-nondirectory x)
                          x))
                  recentf-list))
         (filename-list
          (remove-duplicates (mapcar #'car file-assoc-list)
                             :test #'string=))
         (filename (ido-completing-read "Choose recent file: "
                                        filename-list
                                        nil
                                        t)))
    (when filename
      (find-file (cdr (assoc filename
                             file-assoc-list))))))

(add-to-list 'ido-ignore-files "\\`pyc")


;; Uniqify allows us to tell buffers with similar names apart
(setq uniquify-buffer-name-style 'forward)
(require 'uniquify)

(global-set-key (kbd "<f6>") 'direx-project:jump-to-project-root)
(global-set-key (kbd "<f7>") 'list-bookmarks)


;; MOVING BETWEEN WINDOWS
(global-set-key (kbd "C-<left>") 'windmove-left)
(global-set-key (kbd "C-<right>") 'windmove-right)
(global-set-key (kbd "C-<up>") 'windmove-up)
(global-set-key (kbd "C-<down>") 'windmove-down)

;; PREVIOUS/NEXT BUFFER
(global-set-key (kbd "C-S-<down>") 'previous-buffer)
(global-set-key (kbd "C-S-<up>") 'next-buffer)

;; IDO MODE

(ido-mode t)
(setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point nil
        ido-max-prospects 10)


;; SCROLLING

(setq scroll-margin 3
      scroll-step  1
      scroll-conservatively 10000)


;; GOOGLE
(defun google-this () (interactive)
  (browse-url
   (let ((search-term
          (cond ((region-active-p)
                 (buffer-substring-no-properties region-beginning region-end))
                ((not (eq (word-at-point) nil))
                 (word-at-point))
                ((read-string "Search for term: ")))))
     (concat "http://www.google.co.uk/search?q=" search-term "&ie=utf-8"))))


;; MU4E
(key-chord-define mu4e-compose-mode-map ",x" 'message-kill-buffer)
;; VIMP
(key-chord-define vimp-insert-state-map "kj" 'vimp-normal-state)
