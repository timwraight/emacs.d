;;; vimp-states.el --- States

;; Author: Vegard Øye <vegard_oye at hotmail.com>
;; Maintainer: Vegard Øye <vegard_oye at hotmail.com>

;; Version: 1.2.5

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

;;; Code:

;;; Normal state

(vimp-define-state normal
  "Normal state.
AKA \"Command\" state."
  :tag " <N> "
  :enable (motion)
  :exit-hook (vimp-repeat-start-hook)
  (cond
   ((vimp-normal-state-p)
    (overwrite-mode -1)
    (add-hook 'post-command-hook #'vimp-normal-post-command nil t))
   (t
    (remove-hook 'post-command-hook #'vimp-normal-post-command t))))

(defun vimp-normal-post-command (&optional command)
  "Reset command loop variables in Normal state.
Also prevent point from reaching the end of the line.
If the region is activated, enter Visual state."
  (unless (or (vimp-initializing-p)
              (null this-command))
    (setq command (or command this-command))
    (when (vimp-normal-state-p)
      (setq vimp-this-type nil
            vimp-this-operator nil
            vimp-this-motion nil
            vimp-this-motion-count nil
            vimp-inhibit-operator nil
            vimp-inhibit-operator-value nil)
      (unless (memq command '(vimp-use-register
                              digit-argument
                              negative-argument
                              universal-argument
                              universal-argument-minus
                              universal-argument-more
                              universal-argument-other-key))
        (setq vimp-this-register nil))
      (vimp-adjust-cursor))))
(put 'vimp-normal-post-command 'permanent-local-hook t)

;;; Insert state

(vimp-define-state insert
  "Insert state."
  :tag " <I> "
  :cursor (bar . 2)
  :message "-- INSERT --"
  :entry-hook (vimp-start-track-last-insertion)
  :exit-hook (vimp-cleanup-insert-state vimp-stop-track-last-insertion)
  :input-method t
  (cond
   ((vimp-insert-state-p)
    (add-hook 'pre-command-hook #'vimp-insert-repeat-hook)
    (unless (eq vimp-want-fine-undo t)
      (vimp-start-undo-step t)))
   (t
    (remove-hook 'pre-command-hook #'vimp-insert-repeat-hook)
    (setq vimp-insert-repeat-info vimp-repeat-info)
    (vimp-set-marker ?^ nil t)
    (unless (eq vimp-want-fine-undo t)
      (vimp-end-undo-step t (eq vimp-want-fine-undo 'fine)))
    (when vimp-move-cursor-back
      (when (or (vimp-normal-state-p vimp-next-state)
                (vimp-motion-state-p vimp-next-state))
        (vimp-move-cursor-back))))))

(defun vimp-insert-repeat-hook ()
  "Record insertion keys in `vimp-insert-repeat-info'."
  (setq vimp-insert-repeat-info (last vimp-repeat-info))
  (remove-hook 'pre-command-hook #'vimp-insert-repeat-hook))
(put 'vimp-insert-repeat-hook 'permanent-local-hook t)

(defun vimp-cleanup-insert-state ()
  "Called when Insert state is about to be exited.
Handles the repeat-count of the insertion command."
  (when vimp-insert-count
    (dotimes (i (1- vimp-insert-count))
      (when vimp-insert-lines
        (vimp-insert-newline-below))
      (when (fboundp 'vimp-execute-repeat-info)
        (vimp-execute-repeat-info
         (cdr vimp-insert-repeat-info)))))
  (when vimp-insert-vcount
    (let ((buffer-invisibility-spec buffer-invisibility-spec))
      ;; make all lines hidden by hideshow temporarily visible
      (when (listp buffer-invisibility-spec)
        (setq buffer-invisibility-spec
              (vimp-filter-list
               #'(lambda (x)
                   (or (eq x 'hs)
                       (eq (car-safe x) 'hs)))
               buffer-invisibility-spec)))
      (let ((line (nth 0 vimp-insert-vcount))
            (col (nth 1 vimp-insert-vcount))
            (vcount (nth 2 vimp-insert-vcount)))
        (save-excursion
          (dotimes (v (1- vcount))
            (goto-char (point-min))
            (forward-line (+ line v))
            (when (or (not vimp-insert-skip-empty-lines)
                      (not (integerp col))
                      (save-excursion
                        (vimp-move-end-of-line)
                        (>= (current-column) col)))
              (if (integerp col)
                  (move-to-column col t)
                (funcall col))
              (dotimes (i (or vimp-insert-count 1))
                (when (fboundp 'vimp-execute-repeat-info)
                  (vimp-execute-repeat-info
                   (cdr vimp-insert-repeat-info)))))))))))

;;; Visual state

;; Visual selections are implemented in terms of types, and are
;; compatible with the Emacs region. This is achieved by "translating"
;; the region to the selected text right before a command is executed.
;; If the command is a motion, the translation is postponed until a
;; non-motion command is invoked (distinguished by the :keep-visual
;; command property).
;;
;; Visual state activates the region, enabling Transient Mark mode if
;; not already enabled. This is only temporay: if Transient Mark mode
;; was disabled before entering Visual state, it is disabled when
;; exiting Visual state. This allows Visual state to harness the
;; "transient" behavior of many commands without overriding the user's
;; preferences in other states.

(defmacro vimp-define-visual-selection (selection doc &rest body)
  "Define a Visual selection SELECTION.
Creates a command vimp-visual-SELECTION for enabling the selection.
DOC is the function's documentation string. The following keywords
may be specified in BODY:

:message STRING         Status message when enabling the selection.
:type TYPE              Type to use (defaults to SELECTION).

Following the keywords is optional code which is executed each time
the selection is enabled.

\(fn SELECTION DOC [[KEY VAL]...] BODY...)"
  (declare (indent defun)
           (debug (&define name stringp
                           [&rest keywordp sexp]
                           def-body)))
  (let* ((name (intern (format "vimp-visual-%s" selection)))
         (message (intern (format "%s-message" name)))
         (type selection)
         arg key string)
    ;; collect keywords
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body))
      (cond
       ((eq key :message)
        (setq string arg))
       ((eq key :type)
        (setq type arg))))
    ;; macro expansion
    `(progn
       (add-to-list 'vimp-visual-alist (cons ',selection ',name))
       (defvar ,name ',type ,(format "*%s" doc))
       (defvar ,message ,string ,doc)
       (vimp-define-command ,name (&optional mark point type message)
         ,@(when doc `(,doc))
         :keep-visual t
         :repeat nil
         (interactive
          (list nil nil
                (if (and (vimp-visual-state-p)
                         (eq vimp-visual-selection ',selection))
                    'exit ,name) t))
         (if (eq type 'exit)
             (vimp-exit-visual-state)
           (setq type (or type ,name)
                 vimp-visual-selection ',selection)
           (vimp-visual-make-region mark point type message)
           ,@body))
       ',selection)))

(vimp-define-visual-selection char
  "Characterwise selection."
  :type inclusive
  :message "-- VISUAL --")

(vimp-define-visual-selection line
  "Linewise selection."
  :message "-- VISUAL LINE --")

(vimp-define-visual-selection block
  "Blockwise selection."
  :message "-- VISUAL BLOCK --"
  (vimp-transient-mark -1)
  ;; refresh the :corner property
  (setq vimp-visual-properties
        (plist-put vimp-visual-properties :corner
                   (vimp-visual-block-corner 'upper-left))))

(vimp-define-state visual
  "Visual state."
  :tag " <V> "
  :enable (motion normal)
  :message 'vimp-visual-message
  (cond
   ((vimp-visual-state-p)
    (vimp-save-transient-mark-mode)
    (setq select-active-regions nil)
    (cond
     ((region-active-p)
      (if (< (vimp-visual-direction) 0)
          (vimp-visual-select (region-beginning) (region-end)
                              vimp-visual-char
                              (vimp-visual-direction))
        (vimp-visual-make-selection (mark t) (point)
                                    vimp-visual-char))
      (vimp-visual-highlight))
     (t
      (vimp-visual-make-region (point) (point) vimp-visual-char)))
    (add-hook 'pre-command-hook #'vimp-visual-pre-command nil t)
    (add-hook 'post-command-hook #'vimp-visual-post-command nil t)
    (add-hook 'deactivate-mark-hook #'vimp-visual-deactivate-hook nil t))
   (t
    ;; Postpone deactivation of region if next state is Insert.
    ;; This gives certain insertion commands (auto-pairing characters,
    ;; for example) an opportunity to access the region.
    (if (and (eq vimp-next-state 'insert)
             (eq vimp-visual-selection 'char))
        (add-hook 'vimp-normal-state-entry-hook
                  #'vimp-visual-deactivate-hook nil t)
      (vimp-visual-deactivate-hook))
    (setq vimp-visual-region-expanded nil)
    (remove-hook 'pre-command-hook #'vimp-visual-pre-command t)
    (remove-hook 'post-command-hook #'vimp-visual-post-command t)
    (remove-hook 'deactivate-mark-hook #'vimp-visual-deactivate-hook t)
    (vimp-visual-highlight -1))))

(defun vimp-visual-pre-command (&optional command)
  "Run before each COMMAND in Visual state.
Expand the region to the selection unless COMMAND is a motion."
  (when (vimp-visual-state-p)
    (setq command (or command this-command))
    (when vimp-visual-x-select-timer
      (cancel-timer vimp-visual-x-select-timer))
    (unless (vimp-get-command-property command :keep-visual)
      (vimp-visual-update-x-selection)
      (vimp-visual-expand-region
       ;; exclude final newline from linewise selection
       ;; unless the command has real need of it
       (and (eq (vimp-visual-type) 'line)
            (vimp-get-command-property command :exclude-newline))))))

(put 'vimp-visual-pre-command 'permanent-local-hook t)

(defun vimp-visual-post-command (&optional command)
  "Run after each COMMAND in Visual state.
If COMMAND is a motion, refresh the selection;
otherwise exit Visual state."
  (when (vimp-visual-state-p)
    (setq command (or command this-command))
    (if (or quit-flag
            (eq command #'keyboard-quit)
            ;; Is `mark-active' nil for an unexpanded region?
            deactivate-mark
            (and (not vimp-visual-region-expanded)
                 (not (region-active-p))
                 (not (eq vimp-visual-selection 'block))))
        (progn
          (vimp-exit-visual-state)
          (vimp-adjust-cursor))
      (if vimp-visual-region-expanded
          (vimp-visual-contract-region)
        (vimp-visual-refresh))
      (setq vimp-visual-x-select-timer
            (run-with-idle-timer vimp-visual-x-select-timeout nil
                                 #'vimp-visual-update-x-selection
                                 (current-buffer)))
      (vimp-visual-highlight))))
(put 'vimp-visual-post-command 'permanent-local-hook t)

(defun vimp-visual-update-x-selection (&optional buffer)
  "Update the X selection with the current visual region."
  (let ((buf (or buffer (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (and (vimp-visual-state-p)
                   (fboundp 'x-select-text)
                   (or (not (boundp 'ns-initialized))
                       (with-no-warnings ns-initialized))
                   (not (eq vimp-visual-selection 'block)))
          (x-select-text (buffer-substring-no-properties
                          vimp-visual-beginning
                          vimp-visual-end)))))))

(defun vimp-visual-activate-hook (&optional command)
  "Enable Visual state if the region is activated."
  (unless (vimp-visual-state-p)
    (vimp-delay nil
        ;; the activation may only be momentary, so re-check
        ;; in `post-command-hook' before entering Visual state
        '(unless (or (vimp-visual-state-p)
                     (vimp-insert-state-p)
                     (vimp-emacs-state-p))
           (when (and (region-active-p)
                      (not deactivate-mark))
             (vimp-visual-state)))
      'post-command-hook nil t
      "vimp-activate-visual-state")))
(put 'vimp-visual-activate-hook 'permanent-local-hook t)

(defun vimp-visual-deactivate-hook (&optional command)
  "Deactivate the region and restore Transient Mark mode."
  (setq command (or command this-command))
  (remove-hook 'deactivate-mark-hook
               #'vimp-visual-deactivate-hook t)
  (remove-hook 'vimp-normal-state-entry-hook
               #'vimp-visual-deactivate-hook t)
  (cond
   ((and (vimp-visual-state-p) command
         (not (vimp-get-command-property command :keep-visual)))
    (setq vimp-visual-region-expanded nil)
    (vimp-exit-visual-state))
   ((not (vimp-visual-state-p))
    (vimp-active-region -1)
    (vimp-restore-transient-mark-mode))))
(put 'vimp-visual-deactivate-hook 'permanent-local-hook t)

(vimp-define-command vimp-exit-visual-state (&optional later buffer)
  "Exit from Visual state to the previous state.
If LATER is non-nil, exit after the current command."
  :keep-visual t
  :repeat abort
  (with-current-buffer (or buffer (current-buffer))
    (when (vimp-visual-state-p)
      (if later
          (setq deactivate-mark t)
        (when vimp-visual-region-expanded
          (vimp-visual-contract-region))
        (vimp-change-to-previous-state)))))

(defun vimp-visual-message (&optional selection)
  "Create an echo area message for SELECTION.
SELECTION is a kind of selection as defined by
`vimp-define-visual-selection', such as `char', `line'
or `block'."
  (let (message)
    (setq selection (or selection vimp-visual-selection))
    (when selection
      (setq message
            (symbol-value (intern (format "vimp-visual-%s-message"
                                          selection))))
      (cond
       ((functionp message)
        (funcall message))
       ((stringp message)
        (vimp-echo "%s" message))))))

(defun vimp-visual-select (beg end &optional type dir message)
  "Create a Visual selection of type TYPE from BEG to END.
Point and mark are positioned so that the resulting selection
has the specified boundaries. If DIR is negative, point precedes mark,
otherwise it succedes it. To specify point and mark directly,
use `vimp-visual-make-selection'."
  (let* ((range (vimp-contract beg end type))
         (mark (vimp-range-beginning range))
         (point (vimp-range-end range))
         (dir (or dir 1)))
    (when (< dir 0)
      (vimp-swap mark point))
    (vimp-visual-make-selection mark point type message)))

(defun vimp-visual-make-selection (mark point &optional type message)
  "Create a Visual selection with point at POINT and mark at MARK.
The boundaries of the selection are inferred from these
and the current TYPE. To specify the boundaries and infer
mark and point, use `vimp-visual-select' instead."
  (let* ((selection (vimp-visual-selection-for-type type))
         (func (vimp-visual-selection-function selection))
         (prev (and (vimp-visual-state-p) vimp-visual-selection))
         (mark (vimp-normalize-position mark))
         (point (vimp-normalize-position point))
         (state vimp-state))
    (unless (vimp-visual-state-p)
      (vimp-visual-state))
    (setq vimp-visual-selection selection)
    (funcall func mark point type
             ;; signal a message when changing the selection
             (when (or (not (vimp-visual-state-p state))
                       (not (eq selection prev)))
               message))))

(defun vimp-visual-make-region (mark point &optional type message)
  "Create an active region from MARK to POINT.
If TYPE is given, also set the Visual type.
If MESSAGE is given, display it in the echo area."
  (interactive)
  (let* ((point (vimp-normalize-position
                 (or point (point))))
         (mark (vimp-normalize-position
                (or mark
                    (when (or (vimp-visual-state-p)
                              (region-active-p))
                      (mark t))
                    point))))
    (unless (vimp-visual-state-p)
      (vimp-visual-state))
    (vimp-active-region 1)
    (setq vimp-visual-region-expanded nil)
    (vimp-visual-refresh mark point type)
    (cond
     ((null vimp-echo-state))
     ((stringp message)
      (vimp-echo "%s" message))
     (message
      (cond
       ((stringp vimp-visual-state-message)
        (vimp-echo "%s" vimp-visual-state-message))
       ((functionp vimp-visual-state-message)
        (funcall vimp-visual-state-message)))))))

(defun vimp-visual-expand-region (&optional exclude-newline)
  "Expand the region to the Visual selection.
If EXCLUDE-NEWLINE is non-nil and the selection ends with a newline,
exclude that newline from the region."
  (when (and (vimp-visual-state-p)
             (not vimp-visual-region-expanded))
    (let ((mark vimp-visual-beginning)
          (point vimp-visual-end))
      (when (< vimp-visual-direction 0)
        (vimp-swap mark point))
      (setq vimp-visual-region-expanded t)
      (vimp-visual-refresh mark point)
      (when (and exclude-newline
                 (save-excursion
                   (goto-char vimp-visual-end)
                   (and (bolp) (not (bobp)))))
        (if (< vimp-visual-direction 0)
            (vimp-move-mark (max point (1- (mark))))
          (goto-char (max mark (1- (point)))))))))

(defun vimp-visual-contract-region ()
  "The inverse of `vimp-visual-expand-region'.
Create a Visual selection that expands to the current region."
  (vimp-visual-refresh)
  (setq vimp-visual-region-expanded nil)
  (vimp-visual-refresh vimp-visual-mark vimp-visual-point))

(defun vimp-visual-refresh (&optional mark point type &rest properties)
  "Refresh point, mark and Visual variables.
Refreshes `vimp-visual-beginning', `vimp-visual-end',
`vimp-visual-mark', `vimp-visual-point', `vimp-visual-selection',
`vimp-visual-direction', `vimp-visual-properties' and `vimp-this-type'."
  (let* ((point (or point (point)))
         (mark (or mark (mark t) point))
         (dir (vimp-visual-direction))
         (type (or type (vimp-visual-type vimp-visual-selection)
                   (vimp-visual-type)))
         range)
    (vimp-move-mark mark)
    (goto-char point)
    (setq vimp-visual-beginning
          (or vimp-visual-beginning
              (let ((marker (make-marker)))
                (move-marker marker (min point mark))))
          vimp-visual-end
          (or vimp-visual-end
              (let ((marker (make-marker)))
                (set-marker-insertion-type marker t)
                (move-marker marker (max point mark))))
          vimp-visual-mark
          (or vimp-visual-mark
              (let ((marker (make-marker)))
                (move-marker marker mark)))
          vimp-visual-point
          (or vimp-visual-point
              (let ((marker (make-marker)))
                (move-marker marker point))))
    (setq vimp-visual-properties
          (vimp-concat-plists vimp-visual-properties properties))
    (cond
     (vimp-visual-region-expanded
      (setq type (or (vimp-visual-type) type))
      (move-marker vimp-visual-beginning (min point mark))
      (move-marker vimp-visual-end (max point mark))
      ;; if the type is one-to-one, we can safely refresh
      ;; the unexpanded positions as well
      (when (vimp-type-property type :one-to-one)
        (setq range (apply #'vimp-contract point mark type
                           vimp-visual-properties)
              mark (vimp-range-beginning range)
              point (vimp-range-end range))
        (when (< dir 0)
          (vimp-swap mark point))
        (move-marker vimp-visual-mark mark)
        (move-marker vimp-visual-point point)))
     (t
      (setq range (apply #'vimp-expand point mark type
                         vimp-visual-properties)
            type (vimp-type range type))
      (move-marker vimp-visual-beginning (vimp-range-beginning range))
      (move-marker vimp-visual-end (vimp-range-end range))
      (move-marker vimp-visual-mark mark)
      (move-marker vimp-visual-point point)))
    (setq vimp-visual-direction dir
          vimp-this-type type)))

(defun vimp-visual-highlight (&optional arg)
  "Highlight Visual selection, depending on the Visual type.
With negative ARG, disable highlighting."
  (cond
   ((and (numberp arg) (< arg 1))
    (when vimp-visual-overlay
      (delete-overlay vimp-visual-overlay)
      (setq vimp-visual-overlay nil))
    (when vimp-visual-block-overlays
      (mapc #'delete-overlay vimp-visual-block-overlays)
      (setq vimp-visual-block-overlays nil)))
   ((eq vimp-visual-selection 'block)
    (when vimp-visual-overlay
      (vimp-visual-highlight -1))
    (vimp-visual-highlight-block
     vimp-visual-beginning
     vimp-visual-end))
   (t
    (when vimp-visual-block-overlays
      (vimp-visual-highlight -1))
    (if vimp-visual-overlay
        (move-overlay vimp-visual-overlay
                      vimp-visual-beginning vimp-visual-end)
      (setq vimp-visual-overlay
            (make-overlay vimp-visual-beginning vimp-visual-end)))
    (overlay-put vimp-visual-overlay 'face 'region)
    (overlay-put vimp-visual-overlay 'priority 99))))

(defun vimp-visual-highlight-block (beg end &optional overlays)
  "Highlight rectangular region from BEG to END.
Do this by putting an overlay on each line within the rectangle.
Each overlay extends across all the columns of the rectangle.
Reuse overlays where possible to prevent flicker."
  (let* ((point (point))
         (mark (or (mark t) point))
         (overlays (or overlays 'vimp-visual-block-overlays))
         (old (symbol-value overlays))
         (eol-col (and (memq this-command '(next-line previous-line))
                       (numberp temporary-goal-column)
                       (1+ (min (round temporary-goal-column)
                                (1- most-positive-fixnum)))))
         beg-col end-col new nlines overlay window-beg window-end)
    (save-excursion
      ;; calculate the rectangular region represented by BEG and END,
      ;; but put BEG in the upper-left corner and END in the
      ;; lower-right if not already there
      (setq beg-col (vimp-column beg)
            end-col (vimp-column end))
      (when (>= beg-col end-col)
        (if (= beg-col end-col)
            (setq end-col (1+ end-col))
          (vimp-sort beg-col end-col))
        (setq beg (save-excursion
                    (goto-char beg)
                    (vimp-move-to-column beg-col))
              end (save-excursion
                    (goto-char end)
                    (vimp-move-to-column end-col 1))))
      ;; update end column with eol-col (extension to eol).
      (when (and eol-col (> eol-col end-col))
        (setq end-col eol-col))
      ;; force a redisplay so we can do reliable window
      ;; BEG/END calculations
      (sit-for 0)
      (setq window-beg (max (window-start) beg)
            window-end (min (window-end) (1+ end))
            nlines (count-lines window-beg
                                (min window-end (point-max))))
      ;; iterate over those lines of the rectangle which are
      ;; visible in the currently selected window
      (goto-char window-beg)
      (dotimes (i nlines)
        (let (before after row-beg row-end)
          ;; beginning of row
          (vimp-move-to-column beg-col)
          (when (< (current-column) beg-col)
            ;; prepend overlay with virtual spaces if unable to
            ;; move directly to the first column
            (setq before
                  (propertize
                   (make-string
                    (- beg-col (current-column)) ?\ )
                   'face
                   (or (get-text-property (1- (point)) 'face)
                       'default))))
          (setq row-beg (point))
          ;; end of row
          (vimp-move-to-column end-col)
          (when (and (not (eolp))
                     (< (current-column) end-col))
            ;; append overlay with virtual spaces if unable to
            ;; move directly to the last column
            (setq after
                  (propertize
                   (make-string
                    (if (= (point) row-beg)
                        (- end-col beg-col)
                      (- end-col (current-column)))
                    ?\ ) 'face 'region))
            ;; place cursor on one of the virtual spaces
            (if (= point row-beg)
                (put-text-property
                 0 (min (length after) 1)
                 'cursor t after)
              (put-text-property
               (max 0 (1- (length after))) (length after)
               'cursor t after)))
          (setq row-end (min (point) (line-end-position)))
          ;; trim old leading overlays
          (while (and old
                      (setq overlay (car old))
                      (< (overlay-start overlay) row-beg)
                      (/= (overlay-end overlay) row-end))
            (delete-overlay overlay)
            (setq old (cdr old)))
          ;; reuse an overlay if possible, otherwise create one
          (cond
           ((and old (setq overlay (car old))
                 (or (= (overlay-start overlay) row-beg)
                     (= (overlay-end overlay) row-end)))
            (move-overlay overlay row-beg row-end)
            (overlay-put overlay 'before-string before)
            (overlay-put overlay 'after-string after)
            (setq new (cons overlay new)
                  old (cdr old)))
           (t
            (setq overlay (make-overlay row-beg row-end))
            (overlay-put overlay 'before-string before)
            (overlay-put overlay 'after-string after)
            (setq new (cons overlay new)))))
        (forward-line 1))
      ;; display overlays
      (dolist (overlay new)
        (overlay-put overlay 'face 'region)
        (overlay-put overlay 'priority 99))
      ;; trim old overlays
      (dolist (overlay old)
        (delete-overlay overlay))
      (set overlays (nreverse new)))))

(defun vimp-visual-range ()
  "Return the Visual selection as a range.
This is a list (BEG END TYPE PROPERTIES...), where BEG is the
beginning of the selection, END is the end of the selection,
TYPE is the selection's type, and PROPERTIES is a property list
of miscellaneous selection attributes."
  (apply #'vimp-range
         vimp-visual-beginning vimp-visual-end
         (vimp-visual-type)
         :expanded t
         vimp-visual-properties))

(defun vimp-visual-direction ()
  "Return direction of Visual selection.
The direction is -1 if point precedes mark and 1 otherwise.
See also the variable `vimp-visual-direction', which holds
the direction of the last selection."
  (let* ((point (point))
         (mark (or (mark t) point)))
    (if (< point mark) -1 1)))

(defun vimp-visual-type (&optional selection)
  "Return the type of the Visual selection.
If SELECTION is specified, return the type of that instead."
  (if (and (null selection) (vimp-visual-state-p))
      (or vimp-this-type (vimp-visual-type vimp-visual-selection))
    (setq selection (or selection vimp-visual-selection))
    (symbol-value (cdr-safe (assq selection vimp-visual-alist)))))

(defun vimp-visual-goto-end ()
  "Go to the last line of the Visual selection.
This position may differ from `vimp-visual-end' depending on
the selection type, and is contained in the selection."
  (let ((range (vimp-contract-range (vimp-visual-range))))
    (goto-char (vimp-range-end range))))

(defun vimp-visual-alist ()
  "Return an association list from types to selection symbols."
  (mapcar #'(lambda (e)
              (cons (symbol-value (cdr-safe e)) (cdr-safe e)))
          vimp-visual-alist))

(defun vimp-visual-selection-function (selection)
  "Return a selection function for TYPE.
Default to `vimp-visual-make-region'."
  (or (cdr-safe (assq selection vimp-visual-alist))
      ;; generic selection function
      'vimp-visual-make-region))

(defun vimp-visual-selection-for-type (type)
  "Return a Visual selection for TYPE."
  (catch 'done
    (dolist (selection vimp-visual-alist)
      (when (eq (symbol-value (cdr selection)) type)
        (throw 'done (car selection))))))

(defun vimp-visual-block-corner (&optional corner point mark)
  "Block corner corresponding to POINT, with MARK in opposite corner.
Depending on POINT and MARK, the return value is `upper-left',
`upper-right', `lower-left' or `lower-right':

        upper-left +---+ upper-right
                   |   |
        lower-left +---+ lower-right

One-column or one-row blocks are ambiguous. In such cases,
the horizontal or vertical component of CORNER is used.
CORNER defaults to `upper-left'."
  (let* ((point (or point (point)))
         (mark (or mark (mark t)))
         (corner (symbol-name
                  (or corner
                      (and (overlayp vimp-visual-overlay)
                           (overlay-get vimp-visual-overlay
                                        :corner))
                      'upper-left)))
         (point-col (vimp-column point))
         (mark-col (vimp-column mark))
         horizontal vertical)
    (cond
     ((= point-col mark-col)
      (setq horizontal
            (or (and (string-match "left\\|right" corner)
                     (match-string 0 corner))
                "left")))
     ((< point-col mark-col)
      (setq horizontal "left"))
     ((> point-col mark-col)
      (setq horizontal "right")))
    (cond
     ((= (line-number-at-pos point)
         (line-number-at-pos mark))
      (setq vertical
            (or (and (string-match "upper\\|lower" corner)
                     (match-string 0 corner))
                "upper")))
     ((< point mark)
      (setq vertical "upper"))
     ((> point mark)
      (setq vertical "lower")))
    (intern (format "%s-%s" vertical horizontal))))

;;; Operator-Pending state

(vimp-define-state operator
  "Operator-Pending state."
  :tag " <O> "
  :cursor vimp-half-cursor
  :enable (vimp-operator-shortcut-map operator motion normal))

(vimp-define-keymap vimp-operator-shortcut-map
  "Keymap for Operator-Pending shortcuts like \"dd\" and \"gqq\"."
  :local t
  (setq vimp-operator-shortcut-map (make-sparse-keymap))
  (vimp-initialize-local-keymaps))

;; the half-height "Operator-Pending cursor" cannot be specified
;; as a static `cursor-type' value, since its height depends on
;; the current font size
(defun vimp-half-cursor ()
  "Change cursor to a half-height box.
\(This is really just a thick horizontal bar.)"
  (let ((height (/ (window-pixel-height) (* (window-height) 2))))
    (setq cursor-type (cons 'hbar height))))

;;; Replace state

(vimp-define-state replace
  "Replace state."
  :tag " <R> "
  :cursor hbar
  :message "-- REPLACE --"
  :input-method t
  (cond
   ((vimp-replace-state-p)
    (overwrite-mode 1)
    (add-hook 'pre-command-hook #'vimp-replace-pre-command nil t)
    (unless (eq vimp-want-fine-undo t)
      (vimp-start-undo-step t)))
   (t
    (overwrite-mode -1)
    (remove-hook 'pre-command-hook #'vimp-replace-pre-command t)
    (unless (eq vimp-want-fine-undo t)
      (vimp-end-undo-step t))
    (when vimp-move-cursor-back
      (vimp-move-cursor-back))))
  (setq vimp-replace-alist nil))

(defun vimp-replace-pre-command ()
  "Remember the character under point."
  (when (vimp-replace-state-p)
    (unless (assq (point) vimp-replace-alist)
      (add-to-list 'vimp-replace-alist
                   (cons (point)
                         (unless (eolp)
                           (char-after)))))))
(put 'vimp-replace-pre-command 'permanent-local-hook t)

(defun vimp-replace-backspace ()
  "Restore character under cursor."
  (interactive)
  (let (char)
    (backward-char)
    (when (assq (point) vimp-replace-alist)
      (setq char (cdr (assq (point) vimp-replace-alist)))
      (save-excursion
        (delete-char 1)
        (when char
          (insert char))))))

;;; Motion state

(vimp-define-state motion
  "Motion state."
  :tag " <M> "
  :suppress-keymap t)

;;; Emacs state

(vimp-define-state emacs
  "Emacs state."
  :tag " <E> "
  :message "-- EMACS --"
  :input-method t
  :intercept-esc nil)

(provide 'vimp-states)

;;; vimp-states.el ends here
