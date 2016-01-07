;;; vimp-search.el --- Search and substitute

;; Author: Vegard Øye <vegard_oye at hotmail.com>
;; Maintainer: Vegard Øye <vegard_oye at hotmail.com>

;; Version: 1.2.8

;;
;; This file is NOT part of GNU Emacs.

;;; License:

;; This file is part of Evil.
;;
;; Evil is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Evil is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Evil.  If not, see <http://www.gnu.org/licenses/>.

(require 'vimp-core)
(require 'vimp-common)
(require 'vimp-ex)

;;; Code:

(defun vimp-select-search-module (option module)
  "Change the search module according to MODULE.
If MODULE is `isearch', then Emacs' isearch module is used.
If MODULE is `vimp-search', then Evil's own interactive
search module is used."
  (let ((search-functions
         '(forward
           backward
           word-forward
           word-backward
           unbounded-word-forward
           unbounded-word-backward
           next
           previous)))
    (dolist (fun search-functions)
      (let ((isearch (intern (format "vimp-search-%s" fun)))
            (vimp-search (intern (format "vimp-ex-search-%s" fun))))
        (if (eq module 'isearch)
            (substitute-key-definition
             vimp-search isearch vimp-motion-state-map)
          (substitute-key-definition
           isearch vimp-search vimp-motion-state-map)))))
  (set-default option module))

;; this customization is here because it requires
;; the knowledge of `vimp-select-search-mode'
(defcustom vimp-search-module 'isearch
  "The search module to be used."
  :type '(radio (const :tag "Emacs built-in isearch." :value isearch)
                (const :tag "Evil interactive search." :value vimp-search))
  :group 'vimp
  :set 'vimp-select-search-module
  :initialize 'vimp-custom-initialize-pending-reset)

(defun vimp-push-search-history (string forward)
  "Push STRING into the appropriate search history (determined by FORWARD)."
  (let* ((history-var (if forward
                          'vimp-search-forward-history
                        'vimp-search-backward-history))
         (history (symbol-value history-var)))
    (unless (equal (car-safe history) string)
      (set history-var (cons string history)))))

(defun vimp-search-incrementally (forward regexp-p)
  "Search incrementally for user-entered text."
  (let ((vimp-search-prompt (vimp-search-prompt forward))
        (isearch-search-fun-function 'vimp-isearch-function)
        (point (point))
        search-nonincremental-instead)
    (setq isearch-forward forward)
    (vimp-save-echo-area
      (vimp-without-input-method-hooks
       ;; set the input method locally rather than globally to ensure that
       ;; isearch clears the input method when it's finished
       (setq current-input-method vimp-input-method)
       (if forward
           (isearch-forward regexp-p)
         (isearch-backward regexp-p))
       (vimp-push-search-history isearch-string forward)
       (setq current-input-method nil))
      (when (/= (point) point)
        ;; position the point at beginning of the match only if the call to
        ;; `isearch' has really moved the point. `isearch' doesn't move the
        ;; point only if "C-g" is hit twice to exit the search, in which case we
        ;; shouldn't move the point either.
        (when (and forward isearch-other-end)
          (goto-char isearch-other-end))
        (when (and (eq point (point))
                   (not (string= isearch-string "")))
          (if forward
              (isearch-repeat-forward)
            (isearch-repeat-backward))
          (isearch-exit)
          (when (and forward isearch-other-end)
            (goto-char isearch-other-end)))
        (vimp-flash-search-pattern
         (vimp-search-message isearch-string forward))))))

(defun vimp-flash-search-pattern (string &optional all)
  "Flash last search matches for duration of `vimp-flash-delay'.
If ALL is non-nil, flash all matches. STRING is a message
to display in the echo area."
  (let ((lazy-highlight-initial-delay 0)
        (isearch-search-fun-function 'vimp-isearch-function)
        (isearch-case-fold-search case-fold-search)
        (disable #'(lambda (&optional arg) (vimp-flash-hook t))))
    (when vimp-flash-timer
      (cancel-timer vimp-flash-timer))
    (unless (or (null string)
                (string= string ""))
      (vimp-echo-area-save)
      (vimp-echo "%s" string)
      (isearch-highlight (match-beginning 0) (match-end 0))
      (when all
        (setq isearch-lazy-highlight-wrapped nil
              isearch-lazy-highlight-start (point)
              isearch-lazy-highlight-end (point))
        (isearch-lazy-highlight-new-loop)
        (unless isearch-lazy-highlight-overlays
          (isearch-lazy-highlight-update)))
      (add-hook 'pre-command-hook #'vimp-flash-hook nil t)
      (add-hook 'vimp-operator-state-exit-hook #'vimp-flash-hook nil t)
      (add-hook 'pre-command-hook #'vimp-clean-isearch-overlays nil t)
      (setq vimp-flash-timer
            (run-at-time vimp-flash-delay nil disable)))))

(defun vimp-clean-isearch-overlays ()
  "Clean isearch overlays unless `this-command' is search."
  (remove-hook 'pre-command-hook #'vimp-clean-isearch-overlays t)
  (unless (memq this-command
                '(vimp-search-backward
                  vimp-search-forward
                  vimp-search-next
                  vimp-search-previous
                  vimp-search-word-backward
                  vimp-search-word-forward))
    (isearch-clean-overlays)))
(put 'vimp-clean-isearch-overlays 'permanent-local-hook t)

(defun vimp-flash-hook (&optional force)
  "Disable hightlighting if `this-command' is not search.
Disable anyway if FORCE is t."
  (when (or force
            ;; to avoid flicker, don't disable highlighting
            ;; if the next command is also a search command
            (not (memq this-command
                       '(vimp-search-backward
                         vimp-search-forward
                         vimp-search-next
                         vimp-search-previous
                         vimp-search-word-backward
                         vimp-search-word-forward))))
    (vimp-echo-area-restore)
    (isearch-dehighlight)
    (setq isearch-lazy-highlight-last-string nil)
    (lazy-highlight-cleanup t)
    (when vimp-flash-timer
      (cancel-timer vimp-flash-timer)))
  (remove-hook 'pre-command-hook #'vimp-flash-hook t)
  (remove-hook 'vimp-operator-state-exit-hook #'vimp-flash-hook t))
(put 'vimp-flash-hook 'permanent-local-hook t)

(defun vimp-search-function (&optional forward regexp-p wrap)
  "Return a search function.
If FORWARD is nil, search backward, otherwise forward.
If REGEXP-P is non-nil, the input is a regular expression.
If WRAP is non-nil, the search wraps around the top or bottom
of the buffer."
  `(lambda (string &optional bound noerror count)
     (let ((start (point))
           (search-fun ',(if regexp-p
                             (if forward
                                 're-search-forward
                               're-search-backward)
                           (if forward
                               'search-forward
                             'search-backward)))
           result)
       (setq result (funcall search-fun string bound
                             ,(if wrap t 'noerror) count))
       (when (and ,wrap (null result))
         (goto-char ,(if forward '(point-min) '(point-max)))
         (unwind-protect
             (setq result (funcall search-fun string bound noerror count))
           (unless result
             (goto-char start))))
       result)))

(defun vimp-isearch-function ()
  "Return a search function for use with isearch.
Based on `isearch-regexp' and `isearch-forward'."
  (vimp-search-function isearch-forward vimp-regexp-search vimp-search-wrap))

(defun vimp-search (string forward &optional regexp-p start)
  "Search for STRING and highlight matches.
If FORWARD is nil, search backward, otherwise forward.
If REGEXP-P is non-nil, STRING is taken to be a regular expression.
START is the position to search from; if unspecified, it is
one more than the current position."
  (when (and (stringp string)
             (not (string= string "")))
    (let* ((orig (point))
           (start (or start
                      (if forward
                          (min (point-max) (1+ orig))
                        orig)))
           (isearch-regexp regexp-p)
           (isearch-forward forward)
           (case-fold-search
            (unless (and search-upper-case
                         (not (isearch-no-upper-case-p string nil)))
              case-fold-search))
           (search-func (vimp-search-function
                         forward regexp-p vimp-search-wrap)))
      ;; no text properties, thank you very much
      (set-text-properties 0 (length string) nil string)
      ;; position to search from
      (goto-char start)
      (setq isearch-string string)
      (isearch-update-ring string regexp-p)
      (condition-case nil
          (funcall search-func string)
        (search-failed
         (goto-char orig)
         (user-error "\"%s\": %s not found"
                     string (if regexp-p "pattern" "string"))))
      ;; handle opening and closing of invisible area
      (cond
       ((boundp 'isearch-filter-predicates)
        (dolist (pred isearch-filter-predicates)
          (funcall pred (match-beginning 0) (match-end 0))))
       ((boundp 'isearch-filter-predicate)
        (funcall isearch-filter-predicate (match-beginning 0) (match-end 0))))
      ;; always position point at the beginning of the match
      (goto-char (match-beginning 0))
      ;; determine message for echo area
      (cond
       ((and forward (< (point) start))
        (setq string "Search wrapped around BOTTOM of buffer"))
       ((and (not forward) (> (point) start))
        (setq string "Search wrapped around TOP of buffer"))
       (t
        (setq string (vimp-search-message string forward))))
      (vimp-flash-search-pattern string t))))

(defun vimp-search-word (forward unbounded symbol)
  "Search for word near point.
If FORWARD is nil, search backward, otherwise forward. If SYMBOL
is non-nil then the functions searches for the symbol at point,
otherwise for the word at point."
  (let ((string (car-safe regexp-search-ring))
        (move (if forward #'forward-char #'backward-char))
        (end (if forward #'eobp #'bobp)))
    (setq isearch-forward forward)
    (cond
     ((and (memq last-command
                 '(vimp-search-word-forward
                   vimp-search-word-backward))
           (stringp string)
           (not (string= string "")))
      (vimp-search string forward t))
     (t
      (setq string (vimp-find-thing forward (if symbol 'symbol 'vimp-word)))
      (cond
       ((null string)
        (user-error "No word under point"))
       (unbounded
        (setq string (regexp-quote string)))
       (t
        (setq string
              (format (if symbol "\\_<%s\\_>" "\\<%s\\>")
                      (regexp-quote string)))))
      (vimp-push-search-history string forward)
      (vimp-search string forward t)))))

(defun vimp-find-thing (forward thing)
  "Return THING near point as a string.
THING should be a symbol understood by `thing-at-point',
e.g. 'symbol or 'word.  If FORWARD is nil, search backward,
otherwise forward.  Returns nil if nothing is found."
  (let ((move (if forward #'forward-char #'backward-char))
        (end (if forward #'eobp #'bobp))
        string)
    (save-excursion
      (setq string (thing-at-point thing))
      ;; if there's nothing under point, go forwards
      ;; (or backwards) to find it
      (while (and (null string) (not (funcall end)))
        (funcall move)
        (setq string (thing-at-point thing)))
      (when (stringp string)
        (set-text-properties 0 (length string) nil string))
      (when (> (length string) 0)
        string))))

(defun vimp-find-word (forward)
  "Return word near point as a string.
If FORWARD is nil, search backward, otherwise forward.  Returns
nil if nothing is found."
  (vimp-find-thing forward 'word))

(defun vimp-find-symbol (forward)
  "Return word near point as a string.
If FORWARD is nil, search backward, otherwise forward.  Returns
nil if nothing is found."
  (vimp-find-thing forward 'symbol))

(defun vimp-search-prompt (forward)
  "Return the search prompt for the given direction."
  (if forward "/" "?"))

(defun vimp-search-message (string forward)
  "Prefix STRING with the search prompt."
  (format "%s%s" (vimp-search-prompt forward) string))

(defadvice isearch-message-prefix (around vimp activate)
  "Use `vimp-search-prompt'."
  (if vimp-search-prompt
      (setq ad-return-value vimp-search-prompt)
    ad-do-it))

(defadvice isearch-delete-char (around vimp activate)
  "Exit search if no search string."
  (cond
   ((and vimp-search-prompt (string= isearch-string ""))
    (let (search-nonincremental-instead)
      (setq isearch-success nil)
      (isearch-exit)))
   (t
    ad-do-it)))

(defadvice isearch-lazy-highlight-search (around vimp activate)
  "Never wrap the search in this context."
  (let (vimp-search-wrap)
    ad-do-it))

;;; Ex search

(defun vimp-ex-regex-without-case (re)
  "Return the regular expression without all occurrences of \\c and \\C."
  (vimp-transform-regexp re '((?c . "") (?C . ""))))

(defun vimp-ex-regex-case (re default-case)
  "Return the case as implied by \\c or \\C in regular expression RE.
If \\c appears anywhere in the pattern, the pattern is case
insensitive. If \\C appears, the pattern is case sensitive.
Only the first occurrence of \\c or \\C is used, all others are
ignored. If neither \\c nor \\C appears in the pattern, the case
specified by DEFAULT-CASE is used. DEFAULT-CASE should be either
`sensitive', `insensitive' or `smart'. In the latter case, the pattern
will be case-sensitive if and only if it contains an upper-case
letter, otherwise it will be case-insensitive."
  (cond
   ((string-match "\\(?:^\\|[^\\\\]\\)\\(?:\\\\\\\\\\)*\\\\\\([cC]\\)" re)
    (if (eq (aref (match-string 1 re) 0) ?c) 'insensitive 'sensitive))
   ((eq default-case 'smart)
    (if (isearch-no-upper-case-p re t)
        'insensitive
      'sensitive))
   (t default-case)))

;; a pattern
(defun vimp-ex-make-substitute-pattern (regexp flags)
  "Creates a PATTERN for substitution with FLAGS.
This function respects the values of `vimp-ex-substitute-case'
and `vimp-ex-substitute-global'."
  (vimp-ex-make-pattern regexp
                        (cond
                         ((memq ?i flags) 'insensitive)
                         ((memq ?I flags) 'sensitive)
                         ((not vimp-ex-substitute-case)
                          vimp-ex-search-case)
                         (t vimp-ex-substitute-case))
                        (or (and vimp-ex-substitute-global
                                 (not (memq ?g flags)))
                            (and (not vimp-ex-substitute-global)
                                 (memq ?g flags)))))

(defun vimp-ex-make-search-pattern (regexp)
  "Creates a PATTERN for search.
This function respects the values of `vimp-ex-search-case'."
  (vimp-ex-make-pattern regexp vimp-ex-search-case t))

(defun vimp-ex-make-pattern (regexp case whole-line)
  "Create a new search pattern.
REGEXP is the regular expression to be searched for. CASE should
be either 'sensitive, 'insensitive for case-sensitive and
case-insensitive search, respectively, or anything else.  In the
latter case the pattern is smart-case, i.e. it is automatically
sensitive of the pattern contains one upper case letter,
otherwise it is insensitive.  The input REGEXP is considered a
Vim-style regular expression if `vimp-ex-search-vim-style-regexp'
is non-nil, in which case it is transformed to an Emacs style
regular expression (i.e. certain backslash-codes are
transformed. Otherwise REGEXP must be an Emacs style regular
expression and is not transformed."
  (let ((re (vimp-ex-regex-without-case regexp))
        (ignore-case (eq (vimp-ex-regex-case regexp case) 'insensitive)))
    ;; possibly transform regular expression from vim-style to
    ;; Emacs-style.
    (if vimp-ex-search-vim-style-regexp
        (setq re (vimp-transform-vim-style-regexp re))
      ;; Even for Emacs regular expressions we translate certain
      ;; whitespace sequences
      (setq re (vimp-transform-regexp re
                                      '((?t . "\t")
                                        (?n . "\n")
                                        (?r . "\r")))))
    (list re ignore-case whole-line)))

(defun vimp-ex-pattern-regex (pattern)
  "Return the regular expression of a search PATTERN."
  (nth 0 pattern))

(defun vimp-ex-pattern-ignore-case (pattern)
  "Return t if and only if PATTERN should ignore case."
  (nth 1 pattern))

(defun vimp-ex-pattern-whole-line (pattern)
  "Return t if and only if PATTERN should match all occurences of a line.
Otherwise PATTERN matches only the first occurence."
  (nth 2 pattern))

;; Highlight
(defun vimp-ex-make-hl (name &rest args)
  "Create a new highlight object with name NAME and properties ARGS.
The following properties are supported:
:face The face to be used for the highlighting overlays.
:win The window in which the highlighting should be shown.
     Note that the highlight will be visible in all windows showing
     the corresponding buffer, but only the matches visible in the
     specified window will actually be highlighted. If :win is nil,
     the matches in all windows will be highlighted.
:min The minimal buffer position for highlighted matches.
:max The maximal buffer position for highlighted matches.
:match-hook A hook to be called once for each highlight.
            The hook must take two arguments, the highlight and
            the overlay for that highlight.
:update-hook A hook called once after updating the highlighting
             with two arguments, the highlight and a message string
             describing the current match status."
  (unless (symbolp name)
    (user-error "Expected symbol as name of highlight"))
  (let ((face 'vimp-ex-lazy-highlight)
        (win (selected-window))
        min max match-hook update-hook)
    (while args
      (let ((key (pop args))
            (val (pop args)))
        (cond
         ((eq key :face) (setq face val))
         ((eq key :win)  (setq win val))
         ((eq key :min)  (setq min val))
         ((eq key :max)  (setq max val))
         ((eq key :match-hook) (setq match-hook val))
         ((eq key :update-hook) (setq update-hook val))
         (t (user-error "Unexpected keyword: %s" key)))))
    (when (assoc name vimp-ex-active-highlights-alist)
      (vimp-ex-delete-hl name))
    (when (null vimp-ex-active-highlights-alist)
      (add-hook 'window-scroll-functions
                #'vimp-ex-hl-update-highlights-scroll nil t)
      (add-hook 'window-size-change-functions
                #'vimp-ex-hl-update-highlights-resize nil))
    (push (cons name (vector name
                             nil
                             face
                             win
                             min
                             max
                             match-hook
                             update-hook
                             nil))
          vimp-ex-active-highlights-alist)))

(defun vimp-ex-hl-name (hl)
  "Return the name of the highlight HL."
  (aref hl 0))

(defun vimp-ex-hl-pattern (hl)
  "Return the pattern of the highlight HL."
  (aref hl 1))

(defun vimp-ex-hl-set-pattern (hl pattern)
  "Set the pattern of the highlight HL to PATTERN."
  (aset hl 1 pattern))

(defun vimp-ex-hl-face (hl)
  "Return the face of the highlight HL."
  (aref hl 2))

(defun vimp-ex-hl-window (hl)
  "Return the window of the highlight HL."
  (aref hl 3))

(defun vimp-ex-hl-min (hl)
  "Return the minimal buffer position of the highlight HL."
  (aref hl 4))

(defun vimp-ex-hl-set-min (hl min)
  "Set the minimal buffer position of the highlight HL to MIN."
  (aset hl 4 min))

(defun vimp-ex-hl-max (hl)
  "Return the maximal buffer position of the highlight HL."
  (aref hl 5))

(defun vimp-ex-hl-set-max (hl max)
  "Set the minimal buffer position of the highlight HL to MAX."
  (aset hl 5 max))

(defun vimp-ex-hl-match-hook (hl)
  "Return the match-hook of the highlight HL."
  (aref hl 6))

(defun vimp-ex-hl-update-hook (hl)
  "Return the update-hook of the highlight HL."
  (aref hl 7))

(defun vimp-ex-hl-overlays (hl)
  "Return the list of active overlays of the highlight HL."
  (aref hl 8))

(defun vimp-ex-hl-set-overlays (hl overlays)
  "Set the list of active overlays of the highlight HL to OVERLAYS."
  (aset hl 8 overlays))

(defun vimp-ex-delete-hl (name)
  "Remove the highlighting object with a certain NAME."
  (let ((hl (cdr-safe (assoc name vimp-ex-active-highlights-alist))))
    (when hl
      (mapc #'delete-overlay (vimp-ex-hl-overlays hl))
      (setq vimp-ex-active-highlights-alist
            (assq-delete-all name vimp-ex-active-highlights-alist))
      (vimp-ex-hl-update-highlights))
    (when (null vimp-ex-active-highlights-alist)
      (remove-hook 'window-scroll-functions
                   #'vimp-ex-hl-update-highlights-scroll t)
      (remove-hook 'window-size-change-functions
                   #'vimp-ex-hl-update-highlights-resize))))

(defun vimp-ex-hl-active-p (name)
  "Whether the highlight with a certain NAME is active."
  (and (assoc name vimp-ex-active-highlights-alist) t))

(defun vimp-ex-hl-change (name pattern)
  "Set the regular expression of highlight NAME to PATTERN."
  (let ((hl (cdr-safe (assoc name vimp-ex-active-highlights-alist))))
    (when hl
      (vimp-ex-hl-set-pattern hl
                              (if (zerop (length pattern))
                                  nil
                                pattern))
      (vimp-ex-hl-idle-update))))

(defun vimp-ex-hl-set-region (name beg end &optional type)
  "Set minimal and maximal position of highlight NAME to BEG and END."
  (let ((hl (cdr-safe (assoc name vimp-ex-active-highlights-alist))))
    (when hl
      (vimp-ex-hl-set-min hl beg)
      (vimp-ex-hl-set-max hl end)
      (vimp-ex-hl-idle-update))))

(defun vimp-ex-hl-get-max (name)
  "Return the maximal position of the highlight with name NAME."
  (let ((hl (cdr-safe (assoc name vimp-ex-active-highlights-alist))))
    (and hl (vimp-ex-hl-max hl))))

(defun vimp-ex-hl-update-highlights ()
  "Update the overlays of all active highlights."
  (dolist (hl (mapcar #'cdr vimp-ex-active-highlights-alist))
    (let* ((old-ovs (vimp-ex-hl-overlays hl))
           new-ovs
           (pattern (vimp-ex-hl-pattern hl))
           (case-fold-search (vimp-ex-pattern-ignore-case pattern))
           (case-replace case-fold-search)
           (face (vimp-ex-hl-face hl))
           (match-hook (vimp-ex-hl-match-hook hl))
           result)
      (if pattern
          ;; collect all visible ranges
          (let (ranges sranges)
            (dolist (win (if (eq vimp-ex-interactive-search-highlight
                                 'all-windows)
                             (get-buffer-window-list (current-buffer) nil t)
                           (list (vimp-ex-hl-window hl))))
              (let ((beg (max (window-start win)
                              (or (vimp-ex-hl-min hl) (point-min))))
                    (end (min (window-end win)
                              (or (vimp-ex-hl-max hl) (point-max)))))
                (when (< beg end)
                  (push (cons beg end) ranges))))
            (setq ranges
                  (sort ranges #'(lambda (r1 r2) (< (car r1) (car r2)))))
            (while ranges
              (let ((r1 (pop ranges))
                    (r2 (pop ranges)))
                (cond
                 ;; last range
                 ((null r2)
                  (push r1 sranges))
                 ;; ranges overlap, union
                 ((>= (cdr r1) (car r2))
                  (push (cons (car r1)
                              (max (cdr r1) (cdr r2)))
                        ranges))
                 ;; ranges distinct
                 (t
                  (push r1 sranges)
                  (push r2 ranges)))))

            ;; run through all ranges
            (condition-case lossage
                (save-match-data
                  (dolist (r sranges)
                    (let ((beg (car r))
                          (end (cdr r)))
                      (save-excursion
                        (goto-char beg)
                        ;; set the overlays for the current highlight,
                        ;; reusing old overlays (if possible)
                        (while (and (not (eobp))
                                    (vimp-ex-search-find-next-pattern pattern)
                                    (<= (match-end 0) end))
                          (let ((ov (or (pop old-ovs) (make-overlay 0 0))))
                            (move-overlay ov (match-beginning 0) (match-end 0))
                            (overlay-put ov 'face face)
                            (overlay-put ov 'vimp-ex-hl (vimp-ex-hl-name hl))
                            (overlay-put ov 'priority 1000)
                            (push ov new-ovs)
                            (when match-hook (funcall match-hook hl ov)))
                          (cond
                           ((not (vimp-ex-pattern-whole-line pattern))
                            (forward-line))
                           ((= (match-beginning 0) (match-end 0))
                            (forward-char))
                           (t (goto-char (match-end 0))))))))
                  (mapc #'delete-overlay old-ovs)
                  (vimp-ex-hl-set-overlays hl new-ovs)
                  (if (or (null pattern) new-ovs)
                      (setq result t)
                    ;; Maybe the match could just not be found somewhere else?
                    (save-excursion
                      (goto-char (or (vimp-ex-hl-min hl) (point-min)))
                      (if (and (vimp-ex-search-find-next-pattern pattern)
                               (< (match-end 0) (or (vimp-ex-hl-max hl)
                                                    (point-max))))
                          (setq result (format "Match in line %d"
                                               (line-number-at-pos
                                                (match-beginning 0))))
                        (setq result "No match")))))

              (invalid-regexp
               (setq result (cadr lossage)))

              (search-failed
               (setq result (nth 2 lossage)))

              (error
               (setq result (format "%s" (cadr lossage))))

              (user-error
               (setq result (format "%s" (cadr lossage))))))
        ;; no pattern, remove all highlights
        (mapc #'delete-overlay old-ovs)
        (vimp-ex-hl-set-overlays hl new-ovs))
      (when (vimp-ex-hl-update-hook hl)
        (funcall (vimp-ex-hl-update-hook hl) hl result)))))

(defun vimp-ex-search-find-next-pattern (pattern &optional direction)
  "Look for the next occurrence of PATTERN in a certain DIRECTION.
Note that this function ignores the whole-line property of PATTERN."
  (setq direction (or direction 'forward))
  (let ((case-fold-search (vimp-ex-pattern-ignore-case pattern)))
    (cond
     ((eq direction 'forward)
      (re-search-forward (vimp-ex-pattern-regex pattern) nil t))
     ((eq direction 'backward)
      (let* ((pnt (point))
             (ret (re-search-backward (vimp-ex-pattern-regex pattern) nil t))
             (m (and ret (match-data))))
        (if ret
            (forward-char)
          (goto-char (point-min)))
        (let ((fwdret
               (re-search-forward (vimp-ex-pattern-regex pattern) nil t)))
          (cond
           ((and fwdret (< (match-beginning 0) pnt))
            (setq ret fwdret)
            (goto-char (match-beginning 0)))
           (ret
            (set-match-data m)
            (goto-char (match-beginning 0)))
           (t
            (goto-char pnt)
            ret)))))
     (t
      (user-error "Unknown search direction: %s" direction)))))

(defun vimp-ex-hl-idle-update ()
  "Triggers the timer to update the highlights in the current buffer."
  (when (and vimp-ex-interactive-search-highlight
             vimp-ex-active-highlights-alist)
    (when vimp-ex-hl-update-timer
      (cancel-timer vimp-ex-hl-update-timer))
    (setq vimp-ex-hl-update-timer
          (run-at-time vimp-ex-hl-update-delay nil
                       #'vimp-ex-hl-do-update-highlight
                       (current-buffer)))))

(defun vimp-ex-hl-do-update-highlight (&optional buffer)
  "Timer function for updating the highlights."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (vimp-ex-hl-update-highlights)))
  (setq vimp-ex-hl-update-timer nil))

(defun vimp-ex-hl-update-highlights-scroll (win beg)
  "Update highlights after scrolling in some window."
  (with-current-buffer (window-buffer win)
    (vimp-ex-hl-idle-update)))
(put 'vimp-ex-hl-update-highlights-scroll 'permanent-local-hook t)

(defun vimp-ex-hl-update-highlights-resize (frame)
  "Update highlights after resizing a window."
  (let ((buffers (delete-dups (mapcar #'window-buffer (window-list frame)))))
    (dolist (buf buffers)
      (with-current-buffer buf
        (vimp-ex-hl-idle-update)))))
(put 'vimp-ex-hl-update-highlights-resize 'permanent-local-hook t)

;; interactive search
(defun vimp-ex-search-activate-highlight (pattern)
  "Activate highlighting of the search pattern set to PATTERN.
This function does nothing if `vimp-ex-search-interactive' or
`vimp-ex-search-highlight-all' is nil. "
  (when (and vimp-ex-search-interactive vimp-ex-search-highlight-all)
    (with-current-buffer (or vimp-ex-current-buffer (current-buffer))
      (unless (vimp-ex-hl-active-p 'vimp-ex-search)
        (vimp-ex-make-hl 'vimp-ex-search
                         :win (minibuffer-selected-window)))
      (if pattern
          (vimp-ex-hl-change 'vimp-ex-search pattern)))))

(defun vimp-ex-search (&optional count)
  "Search forward or backward COUNT times for the current ex search pattern.
The search pattern is determined by `vimp-ex-search-pattern' and
the direcion is determined by `vimp-ex-search-direction'."
  (setq vimp-ex-search-start-point (point)
        vimp-ex-last-was-search t
        count (or count 1))
  (let ((orig (point))
        wrapped)
    (dotimes (i (or count 1))
      (when (eq vimp-ex-search-direction 'forward)
        (unless (eobp) (forward-char))
        ;; maybe skip end-of-line
        (when (and vimp-move-cursor-back (eolp) (not (eobp)))
          (forward-char)))
      (let ((res (vimp-ex-find-next)))
        (cond
         ((not res)
          (goto-char orig)
          (signal 'search-failed
                  (list (vimp-ex-pattern-regex vimp-ex-search-pattern))))
         ((eq res 'wrapped) (setq wrapped t)))))
    (if wrapped
        (let (message-log-max)
          (message "Search wrapped")))
    (goto-char (match-beginning 0))
    (setq vimp-ex-search-match-beg (match-beginning 0)
          vimp-ex-search-match-end (match-end 0))
    (vimp-ex-search-goto-offset vimp-ex-search-offset)
    (vimp-ex-search-activate-highlight vimp-ex-search-pattern)))

(defun vimp-ex-find-next (&optional pattern direction nowrap)
  "Search for the next occurrence of the PATTERN in DIRECTION.
PATTERN must be created using `vimp-ex-make-pattern', DIRECTION
is either 'forward or 'backward. If NOWRAP is non nil, the search
does not wrap at buffer boundaries. Furthermore this function
only searches invisible text if `search-invisible' is t. If
PATTERN is not specified the current global pattern
`vimp-ex-search-pattern' and if DIRECTION is not specified the
current global direction `vimp-ex-search-direction' is used.
This function returns t if the search was successful, nil if it
was unsuccessful and 'wrapped if the search was successful but
has been wrapped at the buffer boundaries."
  (setq pattern (or pattern vimp-ex-search-pattern)
        direction (or direction vimp-ex-search-direction))
  (unless (and pattern (vimp-ex-pattern-regex pattern))
    (signal 'search-failed (list "No search pattern")))
  (catch 'done
    (let (wrapped)
      (while t
        (let ((search-result (vimp-ex-search-find-next-pattern pattern
                                                               direction)))
          (cond
           ((and search-result
                 (or (eq search-invisible t)
                     (not (isearch-range-invisible
                           (match-beginning 0) (match-end 0)))))
            ;; successful search and not invisible
            (throw 'done (if wrapped 'wrapped t)))
           ((not search-result)
            ;; unsuccessful search
            (if nowrap
                (throw 'done nil)
              (setq nowrap t
                    wrapped t)
              (goto-char (if (eq direction 'forward)
                             (point-min)
                           (point-max)))))))))))

(defun vimp-ex-search-update (pattern offset beg end message)
  "Update the highlighting and info-message for the search pattern.
PATTERN is the search pattern and OFFSET the associated offset.
BEG and END specifiy the current match, MESSAGE is the info
message to be shown. This function does nothing if
`vimp-ex-search-interactive' is nil."
  (when vimp-ex-search-interactive
    (cond
     ((and beg end)
      ;; update overlay
      (if vimp-ex-search-overlay
          (move-overlay vimp-ex-search-overlay beg end)
        (setq vimp-ex-search-overlay
              (make-overlay beg end))
        (overlay-put vimp-ex-search-overlay 'priority 1001)
        (overlay-put vimp-ex-search-overlay 'face 'vimp-ex-search))
      ;; move point
      (goto-char beg)
      (vimp-ex-search-goto-offset offset)
      ;; update highlights
      (when vimp-ex-search-highlight-all
        (vimp-ex-hl-change 'vimp-ex-search pattern)))
     (t
      ;; no match
      (when vimp-ex-search-overlay
        ;; remove overlay
        (delete-overlay vimp-ex-search-overlay)
        (setq vimp-ex-search-overlay nil))
      ;; no highlights
      (when vimp-ex-search-highlight-all
        (vimp-ex-hl-change 'vimp-ex-search nil))
      ;; and go to initial position
      (goto-char vimp-ex-search-start-point)))
    (when (stringp message)
      (vimp-ex-echo "%s" message))))

(defun vimp-ex-search-start-session ()
  "Initialize Ex for interactive search."
  (remove-hook 'minibuffer-setup-hook #'vimp-ex-search-start-session)
  (add-hook 'after-change-functions #'vimp-ex-search-update-pattern nil t)
  (add-hook 'minibuffer-exit-hook #'vimp-ex-search-stop-session)
  (vimp-ex-search-activate-highlight nil))
(put 'vimp-ex-search-start-session 'permanent-local-hook t)

(defun vimp-ex-search-stop-session ()
  "Stop interactive search."
  (with-current-buffer vimp-ex-current-buffer
    ;; TODO: This is a bad fix to remove duplicates. The duplicates
    ;;       exist because `isearch-range-invisible' may add a single
    ;;       overlay multiple times if we are in an unlucky situation
    ;;       of overlapping overlays. This happens in our case because
    ;;       of the overlays that are used for (lazy) highlighting.
    ;;       Perhaps it would be better to disable those overlays
    ;;       temporarily before calling `isearch-range-invisible'.
    (setq isearch-opened-overlays (delete-dups isearch-opened-overlays))
    (isearch-clean-overlays))
  (remove-hook 'minibuffer-exit-hook #'vimp-ex-search-stop-session)
  (remove-hook 'after-change-functions #'vimp-ex-search-update-pattern t)
  (when vimp-ex-search-overlay
    (delete-overlay vimp-ex-search-overlay)
    (setq vimp-ex-search-overlay nil)))
(put 'vimp-ex-search-stop-session 'permanent-local-hook t)

(defun vimp-ex-split-search-pattern (pattern direction)
  "Split PATTERN in regexp, offset and next-pattern parts.
Returns a triple (regexp  offset next-search)."
  (save-match-data
    (if (or (and (eq direction 'forward)
                 (string-match "\\(?:^\\|[^\\\\]\\)\\(?:\\\\\\\\\\)*\\(/\\([^;]*\\)\\(?:;\\([/?].*\\)?\\)?\\)?$"
                               pattern))
            (and (eq direction 'backward)
                 (string-match "\\(?:^\\|[^\\\\]\\)\\(?:\\\\\\\\\\)*\\(\\?\\([^;]*\\)\\(?:;\\([/?].*\\)?\\)?\\)?$"
                               pattern)))
        (list (substring pattern 0 (match-beginning 1))
              (match-string 2 pattern)
              (match-string 3 pattern))
      (list pattern nil nil))))

(defun vimp-ex-search-full-pattern (pattern-string count direction)
  "Search for a full search pattern PATTERN-STRING in DIRECTION.
This function split PATTERN-STRING in
pattern/offset/;next-pattern parts and performs the search in
DIRECTION which must be either 'forward or 'backward. The first
search is repeated COUNT times. If the pattern part of
PATTERN-STRING is empty, the last global pattern stored in
`vimp-ex-search-pattern' is used instead if in addition the
offset part is nil (i.e. no pattern/offset separator), the last
global offset stored in `vimp-ex-search-offset' is used as
offset. The current match data will correspond to the last
successful match.  This function returns a triple (RESULT PATTERN
OFFSET) where RESULT is

  t              the search has been successful without wrap
  'wrap          the search has been successful with wrap
  'empty-pattern the last pattern has been empty
  nil            the search has not been successful

and PATTERN and OFFSET are the last pattern and offset this
function searched for. Note that this function does not handle
any error conditions."
  (setq count (or count 1))
  (catch 'done
    (while t
      (let* ((res (vimp-ex-split-search-pattern pattern-string direction))
             (pat (pop res))
             (offset (pop res))
             (next-pat (pop res)))
        ;; use last pattern of no new pattern has been specified
        (if (not (zerop (length pat)))
            (setq pat (vimp-ex-make-search-pattern pat))
          (setq pat vimp-ex-search-pattern
                offset (or offset vimp-ex-search-offset)))
        (when (zerop (length pat))
          (throw 'done (list 'empty-pattern pat offset)))
        (let (search-result)
          (while (> count 0)
            (let ((result (vimp-ex-find-next pat direction)))
              (if (not result) (setq search-result nil count 0)
                (setq search-result
                      (if (or (eq result 'wrap)
                              (eq search-result 'wrap))
                          'wrap t)
                      count (1- count)))))
          (cond
           ;; search failed
           ((not search-result) (throw 'done (list nil pat offset)))
           ;; no next pattern, search complete
           ((zerop (length next-pat))
            (vimp-ex-search-goto-offset offset)
            (throw 'done (list search-result pat offset)))
           ;; next pattern but empty
           ((= 1 (length next-pat))
            (vimp-ex-search-goto-offset offset)
            (throw 'done (list 'empty-pattern pat offset)))
           ;; next non-empty pattern, next search iteration
           (t
            (vimp-ex-search-goto-offset offset)
            (setq count 1
                  pattern-string (substring next-pat 1)
                  direction (if (= (aref next-pat 0) ?/)
                                'forward
                              'backward)))))))))

(defun vimp-ex-search-update-pattern (beg end range)
  "Update the current search pattern."
  (save-match-data
    (let ((pattern-string (minibuffer-contents)))
      (with-current-buffer vimp-ex-current-buffer
        (with-selected-window (minibuffer-selected-window)
          (goto-char (1+ vimp-ex-search-start-point))
          (condition-case err
              (let* ((result (vimp-ex-search-full-pattern pattern-string
                                                          (or vimp-ex-search-count 1)
                                                          vimp-ex-search-direction))
                     (success (pop result))
                     (pattern (pop result))
                     (offset (pop result)))
                (cond
                 ((eq success 'wrap)
                  (vimp-ex-search-update pattern offset
                                         (match-beginning 0) (match-end 0)
                                         "Wrapped"))
                 ((eq success 'empty-pattern)
                  (vimp-ex-search-update nil nil nil nil nil))
                 (success
                  (vimp-ex-search-update pattern offset
                                         (match-beginning 0) (match-end 0)
                                         nil))
                 (t
                  (vimp-ex-search-update nil nil
                                         nil nil
                                         "search failed"))))
            (invalid-regexp
             (vimp-ex-search-update nil nil nil nil (cadr err)))
            (error
             (vimp-ex-search-update nil nil nil nil (format "%s" err)))))))))
(put 'vimp-ex-search-update-pattern 'permanent-local-hook t)

(defun vimp-ex-search-exit ()
  "Exit interactive search, keeping lazy highlighting active."
  (interactive)
  (vimp-ex-search-stop-session)
  (exit-minibuffer))

(defun vimp-ex-search-abort ()
  "Abort interactive search, disabling lazy highlighting."
  (interactive)
  (vimp-ex-search-stop-session)
  (vimp-ex-delete-hl 'vimp-ex-search)
  (abort-recursive-edit))

(defun vimp-ex-search-goto-offset (offset)
  "Move point according to search OFFSET and set `vimp-this-type' accordingly.
This function assumes that the current match data represents the
current search result."
  (unless (zerop (length offset))
    (let ((beg (match-beginning 0))
          (end (match-end 0)))
      (save-match-data
        (unless
            (string-match
             "^\\([esb]\\)?\\(\\([-+]\\)?\\([0-9]*\\)\\)$"
             offset)
          (user-error "Invalid search offset: %s" offset))
        (let ((count (if (= (match-beginning 4) (match-end 4))
                         (cond
                          ((not (match-beginning 3)) 0)
                          ((= (aref offset (match-beginning 3)) ?+) +1)
                          (t -1))
                       (string-to-number (match-string 2 offset)))))
          (cond
           ((not (match-beginning 1))
            (setq vimp-this-type 'line)
            (forward-line count))
           ((= (aref offset (match-beginning 1)) ?e)
            (goto-char (+ end count -1))
            (setq vimp-this-type 'inclusive))
           ((memq (aref offset (match-beginning 1)) '(?s ?b))
            (goto-char (+ beg count))
            (setq vimp-this-type 'inclusive))))))))

(defun vimp-ex-search-setup ()
  "Hook to initialize the minibuffer for ex search."
  (add-hook 'pre-command-hook #'vimp-ex-remove-default))

(defun vimp-ex-start-search (direction count)
  "Start a new search in a certain DIRECTION."
  ;; store buffer and window where the search started
  (let ((vimp-ex-current-buffer (current-buffer)))
    (setq vimp-ex-search-count count
          vimp-ex-search-direction direction
          vimp-ex-search-start-point (point)
          vimp-ex-last-was-search t)
    (progn
      ;; ensure minibuffer is initialized accordingly
      (add-hook 'minibuffer-setup-hook #'vimp-ex-search-start-session)
      ;; read the search string
      (let* ((minibuffer-local-map vimp-ex-search-keymap)
             (search-string
              (condition-case err
                  (minibuffer-with-setup-hook
                      #'vimp-ex-search-setup
                    (read-string (if (eq vimp-ex-search-direction 'forward)
                                     "/" "?")
                                 (and vimp-ex-search-history
                                      (propertize
                                       (car vimp-ex-search-history)
                                       'face 'shadow))
                                 'vimp-ex-search-history))
                (quit
                 (vimp-ex-search-stop-session)
                 (vimp-ex-delete-hl 'vimp-ex-search)
                 (goto-char vimp-ex-search-start-point)
                 (signal (car err) (cdr err))))))
        ;; pattern entered successful
        (goto-char (1+ vimp-ex-search-start-point))
        (let* ((result
                (vimp-ex-search-full-pattern search-string
                                             vimp-ex-search-count
                                             vimp-ex-search-direction))
               (success (pop result))
               (pattern (pop result))
               (offset (pop result)))
          (setq vimp-ex-search-pattern pattern
                vimp-ex-search-offset offset)
          (cond
           ((memq success '(t wrap))
            (goto-char (match-beginning 0))
            (setq vimp-ex-search-match-beg (match-beginning 0)
                  vimp-ex-search-match-end (match-end 0))
            (vimp-ex-search-goto-offset offset)
            (vimp-push-search-history search-string (eq direction 'forward))
            (unless vimp-ex-search-persistent-highlight
              (vimp-ex-delete-hl 'vimp-ex-search)))
           (t
            (goto-char vimp-ex-search-start-point)
            (vimp-ex-delete-hl 'vimp-ex-search)
            (signal 'search-failed (list search-string)))))))))

(defun vimp-ex-start-word-search (unbounded direction count &optional symbol)
  "Search for the symbol under point.
The search matches the COUNT-th occurrence of the word.  If the
UNBOUNDED argument is nil, the search matches only at symbol
boundaries, otherwise it matches anywhere.  The DIRECTION
argument should be either `forward' or `backward', determining
the search direction. If SYMBOL is non-nil then the functions
searches for the symbol at point, otherwise for the word at
point."
  (let ((string (vimp-find-thing (eq direction 'forward)
                                 (if symbol 'symbol 'word))))
    (if (null string)
        (user-error "No word under point")
      (let ((regex (if unbounded
                       (regexp-quote string)
                     (format (if symbol "\\_<%s\\_>" "\\<%s\\>")
                             (regexp-quote string)))))
        (setq vimp-ex-search-count count
              vimp-ex-search-direction direction
              vimp-ex-search-pattern
              (vimp-ex-make-search-pattern regex)
              vimp-ex-search-offset nil
              vimp-ex-last-was-search t)
        ;; update search history unless this pattern equals the
        ;; previous pattern
        (unless (equal (car-safe vimp-ex-search-history) regex)
          (push regex vimp-ex-search-history))
        (vimp-push-search-history regex (eq direction 'forward)))
      (vimp-ex-delete-hl 'vimp-ex-search)
      (when (fboundp 'vimp-ex-search-next)
        (vimp-ex-search-next count)))))

;; substitute
(vimp-ex-define-argument-type substitution
  "A substitution pattern argument /pattern/replacement/flags.
This handler highlights the pattern of the current substitution."
  :runner
  (lambda (flag &optional arg)
    (with-selected-window (minibuffer-selected-window)
      (with-current-buffer vimp-ex-current-buffer
        (cond
         ((eq flag 'start)
          (vimp-ex-make-hl
           'vimp-ex-substitute
           :face 'vimp-ex-substitute-matches
           :update-hook #'vimp-ex-pattern-update-ex-info
           :match-hook (and vimp-ex-substitute-interactive-replace
                            #'vimp-ex-pattern-update-replacement))
          (setq flag 'update))

         ((eq flag 'stop)
          (vimp-ex-delete-hl 'vimp-ex-substitute))))

      (when (and (eq flag 'update)
                 vimp-ex-substitute-highlight-all
                 (not (zerop (length arg))))
        (condition-case lossage
            (let* ((result (vimp-ex-get-substitute-info arg t))
                   (pattern (pop result))
                   (replacement (pop result))
                   (range (or (vimp-copy-range vimp-ex-range)
                              (vimp-range (line-beginning-position)
                                          (line-end-position)
                                          'line
                                          :expaned t))))
              (setq vimp-ex-substitute-current-replacement replacement)
              (vimp-expand-range range)
              (vimp-ex-hl-set-region 'vimp-ex-substitute
                                     (vimp-range-beginning range)
                                     (vimp-range-end range))
              (vimp-ex-hl-change 'vimp-ex-substitute pattern))
          (end-of-file
           (vimp-ex-pattern-update-ex-info nil
                                           "incomplete replacement"))
          (user-error
           (vimp-ex-pattern-update-ex-info nil
                                           (format "%s" lossage))))))))

(defun vimp-ex-pattern-update-ex-info (hl result)
  "Update the Ex info string."
  (when (stringp result)
    (vimp-ex-echo "%s" result)))

(defun vimp-ex-pattern-update-replacement (hl overlay)
  "Update the replacement display."
  (when (fboundp 'match-substitute-replacement)
    (let ((fixedcase (not case-replace))
          repl)
      (setq repl (if vimp-ex-substitute-current-replacement
                     (vimp-match-substitute-replacement
                      vimp-ex-substitute-current-replacement
                      fixedcase)
                   ""))
      (put-text-property 0 (length repl)
                         'face 'vimp-ex-substitute-replacement
                         repl)
      (overlay-put overlay 'after-string repl))))

(defun vimp-ex-parse-global (string)
  "Parse STRING as a global argument."
  (vimp-delimited-arguments string 2))

(defun vimp-ex-get-substitute-info (string &optional implicit-r)
  "Returns the substitution info of command line STRING.
This function returns a three-element list \(PATTERN REPLACEMENT
FLAGS) consisting of the substitution parts of STRING. PATTERN is
a ex-pattern (see `vimp-ex-make-pattern') and REPLACEMENT in a
compiled replacement expression (see `vimp-compile-replacement').
The information returned is the actual substitution information
w.r.t. to special situations like empty patterns or repetition of
previous substitution commands. If IMPLICIT-R is non-nil, then
the flag 'r' is assumed, i.e. in the case of an empty pattern the
last search pattern is used. This will be used when called from
a :substitute command with arguments."
  (let (pattern replacement flags)
    (cond
     ((or (null string) (string-match "^[a-zA-Z]" string))
      ;; starts with letter so there is no pattern because the
      ;; separator must not be a letter repeat last substitute
      (setq replacement vimp-ex-substitute-replacement)
      ;; flags are everything that is not a white space
      (when (and string (string-match "[^[:space:]]+" string))
        (setq flags (match-string 0 string))))
     (t
      (let ((args (vimp-delimited-arguments string 3)))
        (setq pattern (pop args)
              replacement (pop args)
              flags (pop args))
        ;; if replacment equals "~" use previous replacement
        (if (equal replacement "~")
            (setq replacement vimp-ex-substitute-replacement)
          (setq replacement (vimp-compile-replacement replacement)))
        ;; append implicit "r" flag if required
        (when (and implicit-r (not (memq ?r (append flags nil))))
          (setq flags (concat flags "r"))))))
    ;; if flags equals "&" add previous flags
    (if (and (not (zerop (length flags)))
             (= (aref flags 0) ?&))
        (setq flags (append (substring flags 1)
                            vimp-ex-substitute-flags))
      (setq flags (append flags nil)))
    ;; if no pattern, use previous pattern, either search or
    ;; substitute pattern depending on `vimp-ex-last-was-search' and
    ;; the r flag
    (when (zerop (length pattern))
      (setq pattern
            (if (eq vimp-search-module 'vimp-search)
                (if (and vimp-ex-last-was-search (memq ?r flags))
                    (and vimp-ex-search-pattern
                         (vimp-ex-pattern-regex vimp-ex-search-pattern))
                  (and vimp-ex-substitute-pattern
                       (vimp-ex-pattern-regex vimp-ex-substitute-pattern)))
              (if (eq case-fold-search t)
                  isearch-string
                (concat isearch-string "\\C")))
            flags (remq ?r flags)))
    ;; generate pattern
    (when pattern
      (setq pattern (vimp-ex-make-substitute-pattern pattern flags)))
    (list pattern replacement flags)))

(defun vimp-ex-nohighlight ()
  "Disable the active search highlightings."
  (interactive)
  (vimp-ex-delete-hl 'vimp-ex-substitute)
  (vimp-ex-delete-hl 'vimp-ex-search))

(provide 'vimp-search)

;;; vimp-search.el ends here
