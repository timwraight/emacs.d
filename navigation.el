; NAVIGATION

;; RECENT FILES
(recentf-mode t)
(setq recentf-max-saved-items 100)



;; Uniqify allows us to tell buffers with similar names apart
(setq uniquify-buffer-name-style 'forward)
(require 'uniquify)

(global-set-key (kbd "<f6>") 'direx-project:jump-to-project-root)
(global-set-key (kbd "<f7>") 'list-bookmarks)


;; IDO MODE

(setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point nil
        ido-max-prospects 10)
(add-to-list 'ido-ignore-files "\\`pyc")

(key-chord-define-global ",f" 'helm-find-files)


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


(require 'key-chord)
;; MU4E
(key-chord-define mu4e-compose-mode-map ",x" 'message-kill-buffer)
;; VIMP
(key-chord-define vimp-insert-state-map "kj" 'vimp-normal-state)
