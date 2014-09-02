(require 'org-install)
(require 'org-depend)
;; ORG MODE

; org-compat seems to be needed by the org-clock library
(load-library "org-compat")
(add-to-list 'load-path "~/.emacs.d/vendor/org-mode/lisp")
(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file "~/Dropbox/org/general.org")

; Keywords
(setq org-todo-keywords '((sequence "ACTION(t)" "NEXT(n)" "|" "DONE(d)")
                          (sequence "QUESTION(q)" "|" "ANSWERED(a)")))

(eval-after-load 'org-agenda
  '(progn
     (define-key org-agenda-keymap "i" 'org-agenda-clock-in)
     (define-key org-agenda-keymap "n" 'org-agenda-clock-out)
     (define-key org-agenda-keymap "e" 'next-line)
     (define-key org-agenda-keymap "u" 'previous-line)))

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
(setq org-clock-report)
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)



(setq org-startup-indented 1)
(setq org-use-speed-commands t)
(add-hook 'org-mode-hook 'auto-fill-mode)
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
(setq org-refile-use-outline-path 'full-file-path)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; Don't ask me every time I want to execute some code
(setq org-confirm-babel-evaluate nil)


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
(add-hook 'org-capture-mode-hook 'vimp-insert-state)

; allow alphanumberical markers in lists
(setq org-list-allow-alphabetical t)

;; spelling
(add-hook 'org-mode-hook 'flyspell-mode)

(add-to-list 'org-modules 'org-mac-iCal)

;; org-babel

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (R . t)
   (emacs-lisp . t)
   (python . t)
   (sql . t)
   (plantuml . t)
   (ditaa . t)
   ))

(setq org-ditaa-jar-path "~/bin/ditaa.jar")


; Taskjuggler
(require 'ox-taskjuggler)
(setq org-enforce-todo-dependencies t)
(setq org-taskjuggler-target-version 3.5)
(setq org-taskjuggler-valid-task-attributes)

; Don't show resource allocation in default report
(setq org-taskjuggler-default-reports
  '("textreport report \"Plan\" {
  formats html
  header '== %title =='

  center -8<-
    [#Plan Plan]
    ----
    === Plan ===
    <[report id=\"plan\"]>
    ----
  ->8-
}

# A traditional Gantt chart with a project overview.
taskreport plan \"\" {
  headline \"Project Plan\"
  columns bsi, name, start, end, effort, chart
  loadunit shortauto
  hideresource 1
}

# A graph showing resource allocation. It identifies whether each
# resource is under- or over-allocated for.
resourcereport resourceGraph \"\" {
  headline \"Resource Allocation Graph\"
  columns no, name, effort, weekly
  loadunit shortauto
  hidetask ~(isleaf() & isleaf_())
  sorttasks plan.start.up
}"))


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
