; NAVIGATION


(if (display-graphic-p)
    (progn
        (global-set-key (kbd "C-S-h") 'windmove-left) 
        (global-set-key (kbd "C-S-l") 'windmove-right)
        (global-set-key (kbd "C-S-k") 'windmove-up)
        (global-set-key (kbd "C-S-j") 'windmove-down))
  (progn
        (global-set-key (kbd "C-S-h") 'windmove-left) 
        (global-set-key (kbd "C-S-l") 'windmove-right)
        (global-set-key (kbd "C-S-k") 'windmove-up)
        (global-set-key (kbd "C-S-j") 'windmove-down))
    )

(setq windmove-wrap-around t)

;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;; FILES

;; From https://sites.google.com/site/steveyegge2/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME." (interactive "sNew name: ")
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
	(progn 	 (rename-file name new-name 1) 	 (rename-buffer new-name) 	 (set-visited-file-name new-name) 	 (set-buffer-modified-p nil)))))) 

;; Also from https://sites.google.com/site/steveyegge2/my-dot-emacs-file
(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
	 (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
	 (newname (concat dir "/" name)))
    
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn 	(copy-file filename newname 1) 	(delete-file filename) 	(set-visited-file-name newname) 	(set-buffer-modified-p nil) 	t)))) 



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


(winner-mode 1)
(global-set-key (kbd "C-M-S-s-h") 'winner-undo)
(global-set-key (kbd "C-M-S-s-l") 'winner-redo)


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


;; Marker Visit commands
(define-key vimp-normal-state-map (kbd "SPC") (lambda ()
                (interactive)
                (switch-to-prev-buffer)))

(define-key vimp-normal-state-map (kbd "S-SPC") (lambda ()
                (interactive)
                (switch-to-next-buffer)))

;; Make it work with magit too
(define-key magit-mode-map (kbd "SPC") (lambda ()
                (interactive)
                (switch-to-prev-buffer)))

(define-key magit-mode-map (kbd "S-SPC") (lambda ()
                (interactive)
                (switch-to-next-buffer)))

;; Make it work with compilation mode
(define-key compilation-mode-map (kbd "SPC") (lambda ()
                (interactive)
                (switch-to-prev-buffer)))

(define-key compilation-mode-map (kbd "S-SPC") (lambda ()
                (interactive)
                (switch-to-next-buffer)))

; These should be there too, same deal
 (define-key vimp-normal-state-map (kbd "C-SPC") (lambda ()
                     (interactive)
                     (next-line 10)
                     (vimp-scroll-line-down 10)))

 (define-key vimp-normal-state-map (kbd "C-S-SPC") (lambda ()
                     (interactive)
                     (previous-line 10)
                     (vimp-scroll-line-up 10)))



(define-key vimp-normal-state-map "f" 'ace-jump-mode)
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)


; HELM
(require 'helm-config)
(require 'helm-ls-git)
(setq helm-ls-git-show-abs-or-relative 'relative)
(setq helm-ff-transformer-show-only-basename nil)
(define-key vimp-normal-state-map "m" 'helm-ls-git-ls)
(define-key vimp-normal-state-map ";" 'helm-mini)
(define-key vimp-normal-state-map "'" 'helm-command-prefix)

(setq helm-display-function
      (lambda (buf)
        (switch-to-buffer buf)))

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map [remap pcomplete] 'helm-esh-pcomplete)))


;; ACK

(require 'ack)
(autoload 'pcomplete/ack "pcmpl-ack")
(autoload 'pcomplete/ack-grep "pcmpl-ack")

(vimp-leader/set-key "a" (lambda () (interactive)
                           (ack (concat "ack " (read-string "Search for: " (word-at-point)))
                                (helm-ls-git-root-dir))))


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


