;;; vimp-repeat.el --- Repeat system

;; Author: Frank Fischer <frank.fischer at mathematik.tu-chemnitz.de>
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

;;; Commentary:

;; A repeat begins when leaving Normal state; it ends when re-entering
;; Normal state. The diagram below shows possible routes between
;; Normal state (N), Insert state (I), Visual state (V),
;; Operator-Pending state (O) and Replace state (R). (Emacs state
;; is an exception: nothing is repeated in that state.)
;;                              ___
;;                             /   \
;;                             | R |
;;                             \___/
;;                             ^   |
;;                             |   |
;;               ___           |___V           ___
;;              /   \ <------- /   \ -------> /   \
;;              | V |          | N |          | O |
;;              \___/ -------> \___/ <------- \___/
;;                  |          |   ^          |
;;                  |          |   |          |
;;                  |          V___|          |
;;                  |          /   \          |
;;                  +--------> | I | <--------+
;;                             \___/
;;
;; The recording of a repeat is started in one of two cases: Either a
;; command is about being executed (in pre-command-hook) or normal
;; state is exited. The recording is stopped whenever a command has
;; being completed and vimp is in normal state afterwards. Therefore,
;; a non-inserting command in normal-state is recorded as a single
;; repeat unit. In contrast, if the command leaves normal state and
;; starts insert-state, all commands that are executed until
;; insert-state is left and normal state is reactivated are recorded
;; together in one repeat unit. In other words, a repeat unit consists
;; of all commands that are executed starting and ending in normal
;; state.
;;
;; Not all commands are recored. There are several commands that are
;; completely ignored and other commands that even abort the currently
;; active recording, e.g., commands that change the current buffer.
;;
;; During recording the repeat information is appended to the variable
;; `vimp-repeat-info', which is cleared when the recording
;; starts. This accumulated repeat information is put into the
;; `vimp-repeat-ring' when the recording is finished. The dot command,
;; `\[vimp-repeat]' (`vimp-repeat') replays the most recent entry in
;; the ring, preceeding repeats can be replayed using
;; `\[vimp-repeat-pop]' (`vimp-repeat-pop').
;;
;; Repeat information can be stored in almost arbitrary form. How the
;; repeat information for each single command is recored is determined
;; by the :repeat property of the command. This property has the
;; following interpretation:
;;
;; t         record commands by storing the key-sequence that invoked it
;; nil       ignore this command completely
;; ignore    synonym to nil
;; motion    command is recorded by storing the key-sequence but only in
;;           insert state, otherwise it is ignored.
;; abort     stop recording of repeat information immediately
;; change    record commands by storing buffer changes
;; SYMBOL    if SYMBOL is contained as key in `vimp-repeat-types'
;;           call the corresponding (function-)value, otherwise
;;           call the function associated with SYMBOL. In both
;;           cases the function should take exactly one argument
;;           which is either 'pre or 'post depending on whether
;;           the function is called before or after the execution
;;           of the command.
;;
;; Therefore, using a certain SYMBOL one can write specific repeation
;; functions for each command.
;;
;; Each value of ring `vimp-repeat-info', i.e., each single repeat
;; information must be one of the following two possibilities:
;; If element is a sequence, it is regarded as a key-sequence to
;; be repeated. Otherwise the element must be a list
;; (FUNCTION PARAMS ...) which will be called using
;; (apply FUNCTION PARAMS) whenever this repeat is being executed.
;;
;; A user supplied repeat function can use the functions
;; `vimp-record-repeat' to append further repeat-information of the
;; form described above to `vimp-repeat-info'. See the implementation
;; of `vimp-repeat-keystrokes' and `vimp-repeat-changes' for examples.
;; Those functions are called in different situations before and after
;; the execution of a command. Each function should take one argument
;; which can be either 'pre, 'post, 'pre-operator or 'post-operator
;; specifying when the repeat function has been called. If the command
;; is a usual command the function is called with 'pre before the
;; command is executed and with 'post after the command has been
;; executed.
;;
;; The repeat information is executed with `vimp-execute-repeat-info',
;; which passes key-sequence elements to `execute-kbd-macro' and
;; executes other elements as defined above.  A special version is
;; `vimp-execute-repeat-info-with-count'.  This function works as
;; `vimp-execute-repeat-info', but replaces the count of the first
;; command. This is done by parsing the key-sequence, ignoring all
;; calls to `digit-prefix-argument' and `negative-argument', and
;; prepending the count as a string to the vector of the remaining
;; key-sequence.

(require 'vimp-states)

;;; Code:

(declare-function vimp-visual-state-p "vimp-visual")
(declare-function vimp-visual-range "vimp-visual")
(declare-function vimp-visual-char "vimp-visual")
(declare-function vimp-visual-line "vimp-visual")
(declare-function vimp-visual-block "vimp-visual")

(defmacro vimp-without-repeat (&rest body)
  (declare (indent defun)
           (debug t))
  `(let ((pre-command-hook (remq 'vimp-repeat-pre-hook pre-command-hook))
         (post-command-hook (remq 'vimp-repeat-post-hook post-command-hook)))
     ,@body
     (vimp-repeat-abort)))

(defsubst vimp-repeat-recording-p ()
  "Returns non-nil iff a recording is in progress."
  (eq vimp-recording-repeat t))

(defun vimp-repeat-start ()
  "Start recording a new repeat into `vimp-repeat-info'."
  (vimp-repeat-reset t)
  (vimp-repeat-record-buffer)
  (when (vimp-visual-state-p)
    (let* ((range (vimp-visual-range))
           (beg (vimp-range-beginning range))
           (end (1- (vimp-range-end range)))
           (nfwdlines (- (line-number-at-pos end)
                         (line-number-at-pos beg))))
      (vimp-repeat-record
       (cond
        ((eq vimp-visual-selection 'char)
         (list #'vimp-repeat-visual-char
               nfwdlines
               (- end
                  (if (zerop nfwdlines)
                      beg
                    (save-excursion
                      (goto-char end)
                      (line-beginning-position))))))
        ((eq vimp-visual-selection 'line)
         (list #'vimp-repeat-visual-line nfwdlines))
        ((eq vimp-visual-selection 'block)
         (list #'vimp-repeat-visual-block
               nfwdlines
               (abs (- (vimp-column beg) (vimp-column end))))))))))

(defun vimp-repeat-stop ()
  "Stop recording a repeat.
Update `vimp-repeat-ring' with the accumulated changes
in `vimp-repeat-info' and clear variables."
  (unwind-protect
      (when (vimp-repeat-recording-p)
        (setq vimp-repeat-info
              (vimp-normalize-repeat-info vimp-repeat-info))
        (when (and vimp-repeat-info vimp-repeat-ring)
          (ring-insert vimp-repeat-ring vimp-repeat-info)))
    (vimp-repeat-reset nil)))

(defun vimp-repeat-abort ()
  "Abort current repeation."
  (vimp-repeat-reset 'abort))

(defun vimp-repeat-reset (flag)
  "Clear all repeat recording variables.
Set `vimp-recording-repeat' to FLAG."
  (setq vimp-recording-repeat flag
        vimp-repeat-info nil
        vimp-repeat-buffer nil))

(defsubst vimp-repeat-record-position (&optional pos)
  "Set `vimp-repeat-pos' to POS or point."
  (setq vimp-repeat-pos (or pos (point))))

(defun vimp-repeat-record-buffer ()
  "Set `vimp-repeat-buffer' to the current buffer."
  (unless (minibufferp)
    (setq vimp-repeat-buffer (current-buffer))))

(defmacro vimp-save-repeat-info (&rest body)
  "Execute BODY, protecting the values of repeat variables."
  (declare (indent defun)
           (debug t))
  `(let (vimp-repeat-ring
         vimp-recording-repeat
         vimp-recording-current-command
         vimp-repeat-info
         vimp-repeat-changes
         vimp-repeat-pos
         vimp-repeat-keys
         vimp-repeat-buffer
         this-command
         last-command)
     ,@body))

(defun vimp-repeat-different-buffer-p (&optional strict)
  "Whether the buffer has changed in a repeat.
If STRICT is non-nil, returns t if the previous buffer
is unknown; otherwise returns t only if the previous
buffer is known and different from the current buffer."
  (and (or (buffer-live-p vimp-repeat-buffer) strict)
       (not (minibufferp))
       (not (eq (current-buffer) vimp-repeat-buffer))))

(defun vimp-repeat-type (command &optional default)
  "Return the :repeat property of COMMAND.
If COMMAND doesn't have this property, return DEFAULT."
  (when (functionp command) ; ignore keyboard macros
    (let* ((type (vimp-get-command-property command :repeat default))
           (repeat-type (assq type vimp-repeat-types)))
      (if repeat-type (cdr repeat-type) type))))

(defun vimp-repeat-force-abort-p (repeat-type)
  "Returns non-nil iff the current command should abort the recording of repeat information."
  (or (vimp-repeat-different-buffer-p)           ; ... buffer changed
      (eq repeat-type 'abort)                    ; ... explicitely forced
      (eq vimp-recording-repeat 'abort)          ; ... already aborted
      (vimp-emacs-state-p)                       ; ... in Emacs state
      (and (vimp-mouse-events-p (this-command-keys))  ; ... mouse events
           (eq repeat-type nil))
      (minibufferp)))                            ; ... minibuffer activated

(defun vimp-repeat-record (info)
  "Add INFO to the end of `vimp-repeat-info'."
  (when (vimp-repeat-recording-p)
    (setq vimp-repeat-info (nconc vimp-repeat-info (list info)))))

;; called from `vimp-normal-state-exit-hook'
(defun vimp-repeat-start-hook ()
  "Record a new repeat when exiting Normal state.
Does not record in Emacs state or if the current command
has :repeat nil."
  (when (and (eq (vimp-repeat-type this-command t) t)
             (not (vimp-emacs-state-p)))
    (vimp-repeat-start)))

;; called from `pre-command-hook'
(defun vimp-repeat-pre-hook ()
  "Prepare the current command for recording the repeation."
  (when vimp-local-mode
    (let ((repeat-type (vimp-repeat-type this-command t)))
      (cond
       ;; abort the repeat
       ((vimp-repeat-force-abort-p repeat-type)
        ;; We mark the current record as being aborted, because there
        ;; may be further pre-hooks following before the post-hook is
        ;; called.
        (vimp-repeat-abort))
       ;; ignore those commands completely
       ((null repeat-type))
       ;; record command
       (t
        ;; In normal-state or visual state, each command is a single
        ;; repeation, therefore start a new repeation.
        (when (or (vimp-normal-state-p)
                  (vimp-visual-state-p))
          (vimp-repeat-start))
        (setq vimp-recording-current-command t)
        (funcall repeat-type 'pre))))))
(put 'vimp-repeat-pre-hook 'permanent-local-hook t)

;; called from `post-command-hook'
(defun vimp-repeat-post-hook ()
  "Finish recording of repeat-information for the current-command."
  (when (and vimp-local-mode vimp-recording-repeat)
    (let ((repeat-type (vimp-repeat-type this-command t)))
      (cond
       ;; abort the repeat
       ((vimp-repeat-force-abort-p repeat-type)
        ;; The command has been aborted but is complete, so just reset
        ;; the recording state.
        (vimp-repeat-reset nil))
       ;; ignore if command should not be recorded or the current
       ;; command is not being recorded
       ((or (null repeat-type)
            (not vimp-recording-current-command)))
       ;; record command
       (t
        (funcall repeat-type 'post)
        ;; In normal state, the repeat sequence is complete, so record it.
        (when (vimp-normal-state-p)
          (vimp-repeat-stop)))))
    ;; done with recording the current command
    (setq vimp-recording-current-command nil)))
(put 'vimp-repeat-post-hook 'permanent-local-hook t)

(defun vimp-clear-command-keys ()
  "Clear `this-command-keys' and all information about the current command keys.
Calling this function prevents further recording of the keys that
invoked the current command"
  (clear-this-command-keys t)
  (setq vimp-repeat-keys ""))

(defun vimp-repeat-keystrokes (flag)
  "Repeation recording function for commands that are repeated by keystrokes."
  (cond
   ((eq flag 'pre)
    (when vimp-this-register
      (vimp-repeat-record
       `(set vimp-this-register ,vimp-this-register)))
    (setq vimp-repeat-keys (this-command-keys)))
   ((eq flag 'post)
    (vimp-repeat-record (if (zerop (length (this-command-keys)))
                            vimp-repeat-keys
                          (this-command-keys)))
    ;; erase commands keys to prevent double recording
    (vimp-clear-command-keys))))

(defun vimp-repeat-motion (flag)
  "Repeation for motions. Motions are recorded by keystroke but only in insert state."
  (when (memq vimp-state '(insert replace))
    (vimp-repeat-keystrokes flag)))

(defun vimp-repeat-changes (flag)
  "Repeation recording function for commands that are repeated by buffer changes."
  (cond
   ((eq flag 'pre)
    (add-hook 'after-change-functions #'vimp-repeat-change-hook nil t)
    (vimp-repeat-start-record-changes))
   ((eq flag 'post)
    (remove-hook 'after-change-functions #'vimp-repeat-change-hook t)
    (vimp-repeat-finish-record-changes))))

;; called from the `after-change-functions' hook
(defun vimp-repeat-change-hook (beg end length)
  "Record change information for current command."
  (let ((repeat-type (vimp-repeat-type this-command t)))
    (when (and (vimp-repeat-recording-p)
               (eq repeat-type 'vimp-repeat-changes)
               (not (vimp-emacs-state-p))
               (not (vimp-repeat-different-buffer-p t))
               vimp-state)
      (unless (vimp-repeat-recording-p)
        (vimp-repeat-start))
      (vimp-repeat-record-change (- beg vimp-repeat-pos)
                                 (buffer-substring beg end)
                                 length))))
(put 'vimp-repeat-change-hook 'permanent-local-hook t)

(defun vimp-repeat-record-change (relpos ins ndel)
  "Record the current buffer changes during a repeat.
If CHANGE is specified, it is added to `vimp-repeat-changes'."
  (when (vimp-repeat-recording-p)
    (setq vimp-repeat-changes
          (nconc vimp-repeat-changes (list (list relpos ins ndel))))))

(defun vimp-repeat-start-record-changes ()
  "Starts the recording of a new set of buffer changes."
  (setq vimp-repeat-changes nil)
  (vimp-repeat-record-position))

(defun vimp-repeat-finish-record-changes ()
  "Finishes the recording of buffer changes and records them as repeat."
  (when (vimp-repeat-recording-p)
    (vimp-repeat-record `(vimp-execute-change
                          ,vimp-repeat-changes
                          ,(- (point) vimp-repeat-pos)))
    (setq vimp-repeat-changes nil)))

(defun vimp-repeat-insert-at-point (flag)
  "Repeation recording function for commands that insert text in region.
This records text insertion when a command inserts some text in a
buffer between (point) and (mark)."
  (cond
   ((eq flag 'pre)
    (add-hook 'after-change-functions #'vimp-repeat-insert-at-point-hook nil t))
   ((eq flag 'post)
    (remove-hook 'after-change-functions #'vimp-repeat-insert-at-point-hook t))))

(defun vimp-repeat-insert-at-point-hook (beg end length)
  (let ((repeat-type (vimp-repeat-type this-command t)))
    (when (and (vimp-repeat-recording-p)
               (eq repeat-type 'vimp-repeat-insert-at-point)
               (not (vimp-emacs-state-p))
               (not (vimp-repeat-different-buffer-p t))
               vimp-state)
      (setq vimp-repeat-pos beg)
      (vimp-repeat-record (list 'insert (buffer-substring beg end))))))
(put 'vimp-repeat-insert-at-point-hook 'permanent-local-hook t)

(defun vimp-normalize-repeat-info (repeat-info)
  "Concatenate consecutive arrays in REPEAT-INFO.
Returns a single array."
  (let* ((result (cons nil nil))
         (result-last result)
         cur cur-last)
    (dolist (rep repeat-info)
      (cond
       ((null rep))
       ((arrayp rep)
        (setq rep (listify-key-sequence rep))
        (cond
         (cur
          (setcdr cur-last (cons rep nil))
          (setq cur-last (cdr cur-last)))
         (t
          (setq cur (cons rep nil))
          (setq cur-last cur))))
       (t
        (when cur
          (setcdr result-last (cons (apply #'vconcat cur) nil))
          (setq result-last (cdr result-last))
          (setq cur nil))
        (setcdr result-last (cons rep nil))
        (setq result-last (cdr result-last)))))
    (when cur
      (setcdr result-last (cons (apply #'vconcat cur) nil)))
    (cdr result)))

(defun vimp-repeat-visual-char (nfwdlines nfwdchars)
  "Restores a character visual selection.
If the selection is in a single line, the restored visual
selection covers the same number of characters. If the selection
covers several lines, the restored selection covers the same
number of lines and the same number of characters in the last
line as the original selection."
  (vimp-visual-char)
  (when (> nfwdlines 0)
    (forward-line nfwdlines))
  (forward-char nfwdchars))

(defun vimp-repeat-visual-line (nfwdlines)
  "Restores a character visual selection.
If the selection is in a single line, the restored visual
selection covers the same number of characters. If the selection
covers several lines, the restored selection covers the same
number of lines and the same number of characters in the last
line as the original selection."
  (vimp-visual-line)
  (forward-line nfwdlines))

(defun vimp-repeat-visual-block (nfwdlines nfwdchars)
  "Restores a character visual selection.
If the selection is in a single line, the restored visual
selection covers the same number of characters. If the selection
covers several lines, the restored selection covers the same
number of lines and the same number of characters in the last
line as the original selection."
  (vimp-visual-block)
  (let ((col (current-column)))
    (forward-line nfwdlines)
    (move-to-column (+ col nfwdchars) t)))

(defun vimp-execute-change (changes rel-point)
  "Executes as list of changes.

CHANGES is a list of triples (REL-BEG INSERT-TEXT NDEL).
REL-BEG is the relative position (to point) where the change
takes place. INSERT-TEXT is the text to be inserted at that
position and NDEL the number of characters to be deleted at that
position before insertion.

REL-POINT is the relative position to point before the changed
where point should be placed after all changes."
  (vimp-save-repeat-info
    (let ((point (point)))
      (dolist (change changes)
        (goto-char (+ point (nth 0 change)))
        (delete-char (nth 2 change))
        (insert (nth 1 change)))
      (goto-char (+ point rel-point)))))

(defun vimp-execute-repeat-info (repeat-info)
  "Executes a repeat-information REPEAT-INFO."
  (vimp-save-repeat-info
    (dolist (rep repeat-info)
      (cond
       ((or (arrayp rep) (stringp rep))
        (let ((input-method current-input-method)
              (vimp-input-method nil))
          (deactivate-input-method)
          (unwind-protect
              (execute-kbd-macro rep)
            (activate-input-method input-method))))
       ((consp rep)
        (when (and (= 3 (length rep))
                   (eq (nth 0 rep) 'set)
                   (eq (nth 1 rep) 'vimp-this-register)
                   (>= (nth 2 rep) ?0)
                   (< (nth 2 rep) ?9))
          (setcar (nthcdr 2 rep) (1+ (nth 2 rep))))
        (apply (car rep) (cdr rep)))
       (t
        (error "Unexpected repeat-info: %S" rep))))))

;; TODO: currently we prepend the replacing count before the
;; key-sequence that calls the command. Can we use direct
;; modification of prefix-arg instead? Does it work in
;; conjunction with `execute-kbd-macro'?
(defun vimp-execute-repeat-info-with-count (count repeat-info)
  "Repeat the repeat-information REPEAT-INFO with the count of
the first command replaced by COUNT. The count is replaced if
and only if COUNT is non-nil."
  (vimp-save-repeat-info
    (cond
     ;; do nothing (zero repeating)
     ((and count (zerop count)))
     ;; replace count
     (count
      (let ((vimp-repeat-count count)
            done)
        (while (and repeat-info
                    (arrayp (car repeat-info))
                    (not done))
          (let* ((count-and-cmd (vimp-extract-count (pop repeat-info))))
            (push (vconcat (number-to-string count)
                           (nth 2 count-and-cmd)
                           (nth 3 count-and-cmd))
                  repeat-info)
            (setq done t)))
        (vimp-execute-repeat-info repeat-info)))
     ;; repeat with original count
     (t
      (vimp-execute-repeat-info repeat-info)))))

(vimp-define-command vimp-repeat (count &optional save-point)
  "Repeat the last editing command with count replaced by COUNT.
If SAVE-POINT is non-nil, do not move point."
  :repeat ignore
  :suppress-operator t
  (interactive (list current-prefix-arg
                     (not vimp-repeat-move-cursor)))
  (cond
   ((null vimp-repeat-ring)
    (error "Already executing repeat"))
   (save-point
    (save-excursion
      (vimp-repeat count)))
   (t
    (unwind-protect
        (let ((confirm-kill-emacs t)
              (kill-buffer-hook
               (cons #'(lambda ()
                         (user-error "Cannot delete buffer in repeat command"))
                     kill-buffer-hook))
              (undo-pointer buffer-undo-list))
          (vimp-with-single-undo
            (setq vimp-last-repeat (list (point) count undo-pointer))
            (vimp-execute-repeat-info-with-count
             count (ring-ref vimp-repeat-ring 0))))
      (vimp-normal-state)))))

;; TODO: the same issue concering disabled undos as for `vimp-paste-pop'
(vimp-define-command vimp-repeat-pop (count &optional save-point)
  "Replace the just repeated command with a previously executed command.
Only allowed after `vimp-repeat', `vimp-repeat-pop' or
`vimp-repeat-pop-next'. Uses the same repeat count that
was used for the first repeat.

The COUNT argument inserts the COUNT-th previous kill.
If COUNT is negative, this is a more recent kill."
  :repeat nil
  :suppress-operator t
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (not vimp-repeat-move-cursor)))
  (cond
   ((not (and (eq last-command #'vimp-repeat)
              vimp-last-repeat))
    (user-error "Previous command was not vimp-repeat: %s" last-command))
   (save-point
    (save-excursion
      (vimp-repeat-pop count)))
   (t
    (unless (eq buffer-undo-list (nth 2 vimp-last-repeat))
      (vimp-undo-pop))
    (goto-char (car vimp-last-repeat))
    ;; rotate the repeat-ring
    (while (> count 0)
      (when vimp-repeat-ring
        (ring-insert-at-beginning vimp-repeat-ring
                                  (ring-remove vimp-repeat-ring 0)))
      (setq count (1- count)))
    (while (< count 0)
      (when vimp-repeat-ring
        (ring-insert vimp-repeat-ring
                     (ring-remove vimp-repeat-ring)))
      (setq count (1+ count)))
    (setq this-command #'vimp-repeat)
    (vimp-repeat (cadr vimp-last-repeat)))))

(vimp-define-command vimp-repeat-pop-next (count &optional save-point)
  "Same as `vimp-repeat-pop', but with negative COUNT."
  :repeat nil
  :suppress-operator t
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (not vimp-repeat-move-cursor)))
  (vimp-repeat-pop (- count) save-point))

(defadvice read-key-sequence (before vimp activate)
  "Record `this-command-keys' before it is reset."
  (when (and (vimp-repeat-recording-p)
             vimp-recording-current-command)
    (let ((repeat-type (vimp-repeat-type this-command t)))
      (if (functionp repeat-type)
          (funcall repeat-type 'post)))))

(provide 'vimp-repeat)

;;; vimp-repeat.el ends here
