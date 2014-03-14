;; ORG MODE
; org-compat seems to be needed by the org-clock library
(load-library "org-compat")
(add-to-list 'load-path "~/.emacs.d/vendor/org-mode/lisp")
(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file "~/Dropbox/org/general.org")

; Keywords
(setq org-todo-keywords '((sequence "ACTION(t)" "NEXT(n)" "ONGOING(o)" "|" "DONE(d)")
                          (sequence "QUESTION(q)" "|" "ANSWERED(a)")))


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


(setq org-startup-indented 1)
(setq org-use-speed-commands t)
(add-hook 'org-mode-hook 'auto-fill-mode)
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
(setq org-refile-use-outline-path t)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; Don't ask me every time I want to execute some code
(setq org-confirm-babel-evaluate nil)

(require 'helm-org)

;;;; Refile settings
; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

; Enter insert on capture
(add-hook 'org-capture-mode-hook 'vimp-insert-state)


;; spelling
(add-hook 'org-mode-hook 'flyspell-mode)

; Columns
(setq org-global-properties )
(setq org-columns-default-format " %55ITEM %TODO %17Effort(Estimated Effort){:} %CLOCKSUM")

;; org-babel

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (emacs-lisp . t)
   (python . t)
   (sql . t)
   (plantuml . t)
   (ditaa . t)
   ))

(setq org-ditaa-jar-path "~/bin/ditaa.jar")

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
