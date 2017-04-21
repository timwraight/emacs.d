(require 'org-install)

;; ORG MODE

; org-compat seems to be needed by the org-clock library
(load-library "org-compat")
(add-to-list 'load-path "~/.emacs.d/vendor/org-mode/lisp")
(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file "~/Dropbox/org/general.org")
(setq org-startup-indented t)
; Keywords
(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                          (sequence "QUESTION(q)" "|" "ANSWERED(a)")))

(eval-after-load 'org-agenda
  '(progn
	 (define-key org-agenda-mode-map (kbd "m") 'helm-timi) 
	 (define-key org-agenda-mode-map (kbd "M-m") 'org-agenda-bulk-mark) 
     (define-key org-agenda-keymap "i" 'org-agenda-clock-in)
     (define-key org-agenda-keymap "n" 'org-agenda-clock-out)
     (define-key org-agenda-keymap "e" 'next-line)
     (define-key org-agenda-keymap "u" 'previous-line)
     (define-key org-agenda-keymap (kbd "SPC") evil-leader--default-map)))


(setq org-enforce-todo-dependencies t)

; Persist clock history
(setq org-clock-out-when-done t)
;; Resume clocking task on clock-in if the clock is open
(org-clock-persistence-insinuate)
(setq org-clock-in-resume t)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)

(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archived Tasks")
 
(add-hook 'org-mode-hook 'auto-fill-mode)

(require 's)
(defun two-level-buffer ()
    (s-join "/" (last (split-string (buffer-file-name) "/") 2)))

(setq org-use-speed-commands t)
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-restore-windows-after-quit nil)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

; Hitting return on a link will open it
(setq org-return-follows-link t)

; hide the slashes around emphasised words
(setq org-hide-emphasis-markers t)

(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

(setq org-outline-path-complete-in-steps nil)
(setq org-refile-use-outline-path 'full-file-path)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; Don't ask me every time I want to execute some code
(setq org-confirm-babel-evaluate nil)

(add-to-list 'org-global-properties '("CLOCK_MODELINE_TOTAL" . "today"))

(defun make-subtle-breadcrumbs ()
  ""
  (save-excursion
    (goto-char (point-min))
    ; Are we on a breadcrumb path?
    (while (re-search-forward "»" nil t) 
      (add-face-text-property (point-at-bol) (match-beginning 0) 
                           '(:foreground "#777")))) 
  )


(add-hook 'org-finalize-agenda-hook 'make-subtle-breadcrumbs)


;;;; Refile settings
; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido nil)
;; (setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))
; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)


; Enter insert on capture
(add-hook 'org-capture-mode-hook 'evil-insert-state)

; allow alphanumberical markers in lists
(setq org-list-allow-alphabetical t)

;; spelling
(add-hook 'org-mode-hook 'flyspell-mode)

(add-to-list 'org-modules 'org-mac-iCal)

;; org-babel

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)))
(setq org-ditaa-jar-path "~/bin/ditaa.jar")
(setq org-startup-with-inline-images t)
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

(with-eval-after-load 'vimp
  (evil-define-key 'insert org-mode-map (kbd "M-i") 'org-metaright)
  (evil-define-key 'insert org-mode-map (kbd "M-n") 'org-metaleft)
  (evil-define-key 'insert org-mode-map (kbd "M-u") 'org-metaup)
  (evil-define-key 'insert org-mode-map (kbd "M-e") 'org-metadown)
  )


                                        ; DEADLINES
(setq org-deadline-warning-days 2)


(setq org-latex-packages-alist
      '(("sc" "mathpazo" t)
        ("T1" "fontenc" t)
        ("" "fixltx2e" nil)
        ("" "graphicx" t)
        ("" "longtable" nil)
        ("" "float" nil)
        ("" "wrapfig" nil)
        ("normalem" "ulem" t)
        ("" "textcomp" t)
        ("" "marvosym" t)
        ("" "minted" nil)
        ("" "upquote" nil)
        ("" "wasysym" t)
        ("" "latexsym" t)
        ("" "amssymb" t)
        ("" "amstext" nil)
        ("" "hyperref" nil)
        "\\tolerance=1000"
        "\\linespread{1.05}"
        "\\AtBeginDocument{ \\def\\PYZsq{\\textquotesingle} }"))

(setq org-latex-listings 'minted)
(setq org-latex-pdf-process
      '("latexmk -f -pdf -latexoption=-shell-escape %f"))


(defun tw/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))


(defun tw/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (tw/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun tw/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun tw/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun tw/skip-non-projects ()
  "Skip trees that are not projects"
  ;; (tw/list-sublevels-for-projects-indented)
  (if (save-excursion (tw/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((tw/is-project-p)
            nil)
           ((and (tw/is-project-subtree-p) (not (tw/is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun tw/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (tw/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))


(defun tw/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    ;; Consider only tasks with done todo headings as archivable candidates
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t))))
      (if (member (org-get-todo-state) org-todo-keywords-1)
          (if (member (org-get-todo-state) org-done-keywords)
              (let* ((daynr (string-to-int (format-time-string "%d" (current-time))))
                     (a-month-ago (* 60 60 24 (+ daynr 1)))
                     (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                     (this-month (format-time-string "%Y-%m-" (current-time)))
                     (subtree-is-current (save-excursion
                                           (forward-line 1)
                                           (and (< (point) subtree-end)
                                                (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                (if subtree-is-current
                    subtree-end ; Has a date in this month or last month, skip it
                  nil))  ; available to archive
            (or subtree-end (point-max)))
        next-headline))))


(require 's)
(defun two-level-buffer ()
    (s-join "/" (last (split-string (buffer-file-name) "/") 2)))

(defun item-with-outline ()
  (let*
      ((outline-path (org-get-outline-path)))
    (if (not (eq '() outline-path))
        (concat (org-format-outline-path outline-path) " »")
      "")))



(setq org-agenda-prefix-format
 (quote
  ((agenda . "%-12:c%?-12t% s %b ")
   (timeline . "  % s")
   (todo . " %i %-12:c %(item-with-outline) ")
   (tags .  " %i %-12:c %(item-with-outline) ")
   (search . " %i %-12:c"))))

;; When sorting our agenda items, treat ones without time as being late
(setq org-sort-agenda-notime-is-late nil)

(setq org-agenda-custom-commands
      '(
        (" " "Tim's Agenda"
         ((agenda "")
          (todo "TODO"
                     ((org-agenda-files '("~/org/journal.org"))
                      (org-agenda-overriding-header "Recent work")
                      (org-agenda-sorting-strategy
                       '(timestamp-down priority-down))
          ))
          (tags-todo "-dormant-CANCELLED/TODO|NEXT"
                     ((org-agenda-overriding-header "Tasks")
                      (org-agenda-sorting-strategy
                       '(timestamp-down priority-down))
                      ))
          (tags-todo "/QUESTION"
                     ((org-agenda-overriding-header "Questions")
                      (org-agenda-sorting-strategy
                       '(todo-state-down priority-down))
                      ))
          (tags "-REFILE/"
                ((org-agenda-overriding-header "Tasks to Archive")
                 (org-agenda-skip-function 'tw/skip-non-archivable-tasks)
                 (org-tags-match-list-sublevels nil)
                 ))
          ))
                                        ; 'c' is our prefix for 'custom agendas'
        ("c" . "Custom Agendas")
        ("cr" "Regular Tasks" tags "regular")
        ("cp" "Current Projects" tags "project-dormant+LEVEL=2"
         ((org-agenda-overriding-header "Current Projects")
          (org-agenda-skip-function 'tw/skip-non-projects))
         (org-tags-match-list-sublevels 'indented))
        ("ct" "Project Tasks" tags "project-dormant/TODO|NEXT"
         ((org-agenda-overriding-header "Project Tasks")
          (org-tags-match-list-sublevels 'indented)))
        ("ca" "All my tasks" tags-todo "TODO=\"TODO\"")
        ("cn" "Next Actions" tags-todo "-dormant/TODO=\"NEXT\"")
        ("cw" "This week's tasks" tags-todo "TODO=\"TODO\"+SCHEDULED<=\"<+1w>\"")
        ("cd" "Technical Debt" tags "+technical_debt")
        ))

; Redefine this so that we get breadcrumbs in our clock select menu
(defun org-clock-insert-selection-line (i marker)
  "Insert a line for the clock selection menu.
And return a cons cell with the selection character integer and the marker
pointing to it."
  (when (marker-buffer marker)
    (let (file cat task heading prefix)
      (with-current-buffer (org-base-buffer (marker-buffer marker))
	(save-excursion
	  (save-restriction
	    (widen)
	    (ignore-errors
	      (goto-char marker)
	      (setq file (buffer-file-name (marker-buffer marker))
		    cat (org-get-category)
		    heading (org-get-heading 'notags)
		    prefix (save-excursion
			     (org-back-to-heading t)
			     (looking-at org-outline-regexp)
			     (match-string 0))
		    task (substring
			  (org-fontify-like-in-org-mode
			   (concat prefix heading)
			   org-odd-levels-only)
			  (length prefix)))))))
      (when (and cat task)
	(insert (format "[%c] %-12s  %s\n" i cat task))
	(cons i marker)))))


(global-set-key (kbd "<f10>") 'org-agenda)
(global-set-key (kbd "<f3>") 'org-iswitchb)

(setq org-capture-templates
      '(("t" "Todo" entry (file "~/org/refile.org")
         "* TODO %?\n  %i")
        (" " "Todo today" entry (file "~/org/refile.org")
         "* TODO %?\n  %i \n%t")
        ("q" "Question" entry (file "~/org/refile.org")
         "* QUESTION %?\n  %i")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
             "* %?\nEntered on %U\n "))
      )
