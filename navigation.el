; NAVIGATION

;; RECENT FILES
(recentf-mode t)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
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

(global-set-key (kbd "C-d") 'vimp-scroll-down)
(global-set-key (kbd "C-e") 'vimp-scroll-up)


;; GOOGLE
;(defun google-this () (interactive)
  ;(browse-url
   ;(let ((search-term
          ;(cond ((region-active-p)
                    ;(buffer-substring-no-properties region-beginning region-end))
           ;((not (eq (word-at-point) nil)) (word-at-point))
           ;(read-string "Search for term: ")))))
   ;(concat "http://www.google.co.uk/search?q=" search-term "&ie=utf-8")))

;(vimp-leader/set-key "s" 'google-this)
