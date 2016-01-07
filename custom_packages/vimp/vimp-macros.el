;;; vimp-macros.el --- Macros

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

(require 'vimp-common)
(require 'vimp-states)
(require 'vimp-repeat)

;;; Code:

(declare-function vimp-ex-p "vimp-ex")

;; set some error codes
(put 'beginning-of-line 'error-conditions '(beginning-of-line error))
(put 'beginning-of-line 'error-message "Beginning of line")
(put 'end-of-line 'error-conditions '(end-of-line error))
(put 'end-of-line 'error-message "End of line")

(defun vimp-motion-range (motion &optional count type)
  "Execute a motion and return the buffer positions.
The return value is a list (BEG END TYPE)."
  (let ((opoint   (point))
        (omark    (mark t))
        (omactive (and (boundp 'mark-active) mark-active))
        (obuffer  (current-buffer))
        (vimp-motion-marker (move-marker (make-marker) (point)))
        range)
    (vimp-with-transient-mark-mode
      (vimp-narrow-to-field
        (unwind-protect
            (let ((current-prefix-arg count)
                  ;; Store type in global variable `vimp-this-type'.
                  ;; If necessary, motions can change their type
                  ;; during execution by setting this variable.
                  (vimp-this-type
                   (or type (vimp-type motion 'exclusive))))
              (condition-case err
                  (let ((repeat-type (vimp-repeat-type motion t)))
                    (if (functionp repeat-type)
                        (funcall repeat-type 'pre))
                    (unless (with-local-quit
                              (setq range (call-interactively motion))
                              t)
                      (vimp-repeat-abort)
                      (setq quit-flag t))
                    (if (functionp repeat-type)
                        (funcall repeat-type 'post)))
                (error (prog1 nil
                         (vimp-repeat-abort)
                         ;; some operators depend on succeeding
                         ;; motions, in particular for
                         ;; `vimp-forward-char' (e.g., used by
                         ;; `vimp-substitute'), therefore we let
                         ;; end-of-line and end-of-buffer pass
                         (if (not (memq (car err) '(end-of-line end-of-buffer)))
                             (signal (car err) (cdr err))
                           (message (error-message-string err))))))
              (cond
               ;; the motion returned a range
               ((vimp-range-p range))
               ;; the motion made a Visual selection
               ((vimp-visual-state-p)
                (setq range (vimp-visual-range)))
               ;; the motion made an active region
               ((region-active-p)
                (setq range (vimp-range (region-beginning)
                                        (region-end)
                                        vimp-this-type)))
               ;; default: range from previous position to current
               (t
                (setq range (vimp-expand-range
                             (vimp-normalize vimp-motion-marker
                                             (point)
                                             vimp-this-type)))))
              (unless (or (null type) (eq (vimp-type range) type))
                (vimp-set-type range type)
                (vimp-expand-range range))
              (vimp-set-range-properties range nil)
              range)
          ;; restore point and mark like `save-excursion',
          ;; but only if the motion hasn't disabled the operator
          (unless vimp-inhibit-operator
            (set-buffer obuffer)
            (vimp-move-mark omark)
            (goto-char opoint))
          ;; delete marker so it doesn't slow down editing
          (move-marker vimp-motion-marker nil))))))

(defmacro vimp-define-motion (motion args &rest body)
  "Define an motion command MOTION.

\(fn MOTION (COUNT ARGS...) DOC [[KEY VALUE]...] BODY...)"
  (declare (indent defun)
           (debug (&define name lambda-list
                           [&optional stringp]
                           [&rest keywordp sexp]
                           [&optional ("interactive" [&rest form])]
                           def-body)))
  (let (arg doc interactive key keys type)
    (when args
      (setq args `(&optional ,@(delq '&optional args))
            ;; the count is either numerical or nil
            interactive '("<c>")))
    ;; collect docstring
    (when (and (> (length body) 1)
               (or (eq (car-safe (car-safe body)) 'format)
                   (stringp (car-safe body))))
      (setq doc (pop body)))
    ;; collect keywords
    (setq keys (plist-put keys :repeat 'motion))
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body)
            keys (plist-put keys key arg)))
    ;; collect `interactive' specification
    (when (eq (car-safe (car-safe body)) 'interactive)
      (setq interactive (cdr (pop body))))
    ;; macro expansion
    `(progn
       ;; refresh echo area in Eldoc mode
       (when ',motion
         (eval-after-load 'eldoc
           '(and (fboundp 'eldoc-add-command)
                 (eldoc-add-command ',motion))))
       (vimp-define-command ,motion (,@args)
         ,@(when doc `(,doc))          ; avoid nil before `interactive'
         ,@keys
         :keep-visual t
         (interactive ,@interactive)
         ,@body))))

(defmacro vimp-narrow-to-line (&rest body)
  "Narrow BODY to the current line.
BODY will signal the errors 'beginning-of-line or 'end-of-line
upon reaching the beginning or end of the current line.

\(fn [[KEY VAL]...] BODY...)"
  (declare (indent defun)
           (debug t))
  `(let* ((range (vimp-expand (point) (point) 'line))
          (beg (vimp-range-beginning range))
          (end (vimp-range-end range))
          (min (point-min))
          (max (point-max)))
     (when (save-excursion (goto-char end) (bolp))
       (setq end (max beg (1- end))))
     ;; don't include the newline in Normal state
     (when (and vimp-move-cursor-back
                (not vimp-move-beyond-eol)
                (not (vimp-visual-state-p))
                (not (vimp-operator-state-p)))
       (setq end (max beg (1- end))))
     (vimp-with-restriction beg end
       (vimp-signal-without-movement
         (condition-case err
             (progn ,@body)
           (beginning-of-buffer
            (if (= beg min)
                (signal (car err) (cdr err))
              (signal 'beginning-of-line nil)))
           (end-of-buffer
            (if (= end max)
                (signal (car err) (cdr err))
              (signal 'end-of-line nil))))))))

;; we don't want line boundaries to trigger the debugger
;; when `debug-on-error' is t
(add-to-list 'debug-ignored-errors "^Beginning of line$")
(add-to-list 'debug-ignored-errors "^End of line$")

(defun vimp-eobp (&optional pos)
  "Whether point is at end-of-buffer with regard to end-of-line."
  (save-excursion
    (when pos (goto-char pos))
    (cond
     ((eobp))
     ;; the rest only pertains to Normal state
     ((not (vimp-normal-state-p))
      nil)
     ;; at the end of the last line
     ((eolp)
      (forward-char)
      (eobp))
     ;; at the last character of the last line
     (t
      (forward-char)
      (cond
       ((eobp))
       ((eolp)
        (forward-char)
        (eobp)))))))

(defun vimp-move-beginning (count forward &optional backward)
  "Move to the beginning of the COUNT next object.
If COUNT is negative, move to the COUNT previous object.
FORWARD is a function which moves to the end of the object, and
BACKWARD is a function which moves to the beginning.
If one is unspecified, the other is used with a negative argument."
  (let* ((count (or count 1))
         (backward (or backward
                       #'(lambda (count)
                           (funcall forward (- count)))))
         (forward (or forward
                      #'(lambda (count)
                          (funcall backward (- count)))))
         (opoint (point)))
    (cond
     ((< count 0)
      (when (bobp)
        (signal 'beginning-of-buffer nil))
      (unwind-protect
          (vimp-motion-loop (nil count count)
            (funcall backward 1))
        (unless (zerop count)
          (goto-char (point-min)))))
     ((> count 0)
      (when (vimp-eobp)
        (signal 'end-of-buffer nil))
      ;; Do we need to move past the current object?
      (when (<= (save-excursion
                  (funcall forward 1)
                  (funcall backward 1)
                  (point))
                opoint)
        (setq count (1+ count)))
      (unwind-protect
          (vimp-motion-loop (nil count count)
            (funcall forward 1))
        (if (zerop count)
            ;; go back to beginning of object
            (funcall backward 1)
          (goto-char (point-max)))))
     (t
      count))))

(defun vimp-move-end (count forward &optional backward inclusive)
  "Move to the end of the COUNT next object.
If COUNT is negative, move to the COUNT previous object.
FORWARD is a function which moves to the end of the object, and
BACKWARD is a function which moves to the beginning.
If one is unspecified, the other is used with a negative argument.
If INCLUSIVE is non-nil, then point is placed at the last character
of the object; otherwise it is placed at the end of the object."
  (let* ((count (or count 1))
         (backward (or backward
                       #'(lambda (count)
                           (funcall forward (- count)))))
         (forward (or forward
                      #'(lambda (count)
                          (funcall backward (- count)))))
         (opoint (point)))
    (cond
     ((< count 0)
      (when (bobp)
        (signal 'beginning-of-buffer nil))
      ;; Do we need to move past the current object?
      (when (>= (save-excursion
                  (funcall backward 1)
                  (funcall forward 1)
                  (point))
                (if inclusive
                    (1+ opoint)
                  opoint))
        (setq count (1- count)))
      (unwind-protect
          (vimp-motion-loop (nil count count)
            (funcall backward 1))
        (if (not (zerop count))
            (goto-char (point-min))
          ;; go to end of object
          (funcall forward 1)
          (when inclusive
            (unless (bobp) (backward-char)))
          (when (or (vimp-normal-state-p)
                    (vimp-motion-state-p))
            (vimp-adjust-cursor t)))))
     ((> count 0)
      (when (vimp-eobp)
        (signal 'end-of-buffer nil))
      (when inclusive
        (forward-char))
      (unwind-protect
          (vimp-motion-loop (nil count count)
            (funcall forward 1))
        (if (not (zerop count))
            (goto-char (point-max))
          (when inclusive
            (unless (bobp) (backward-char)))
          (when (or (vimp-normal-state-p)
                    (vimp-motion-state-p))
            (vimp-adjust-cursor t)))))
     (t
      count))))

(defun vimp-text-object-make-linewise (range)
  "Turn the text object selection RANGE to linewise.
The selection is adjusted in a sensible way so that the selected
lines match the user intent. In particular, whitespace-only parts
at the first and last lines are omitted. This function returns
the new range."
  ;; Bug #607
  ;; If new type is linewise and the selection of the
  ;; first line consists of whitespace only, the
  ;; beginning is moved to the start of the next line. If
  ;; the selections of the last line consists of
  ;; whitespace only, the end is moved to the end of the
  ;; previous line.
  (let ((expanded (plist-get (vimp-range-properties range) :expanded))
        (newrange (vimp-expand-range range t)))
    (save-excursion
      ;; skip whitespace at the beginning
      (goto-char (vimp-range-beginning newrange))
      (skip-chars-forward " \t")
      (when (and (not (bolp)) (eolp))
        (vimp-set-range-beginning newrange (1+ (point))))
      ;; skip whitepsace at the end
      (goto-char (vimp-range-end newrange))
      (skip-chars-backward " \t")
      (when (and (not (eolp)) (bolp))
        (vimp-set-range-end newrange (1- (point))))
      ;; only modify range if result is not empty
      (if (> (vimp-range-beginning newrange)
             (vimp-range-end newrange))
          range
        (unless expanded
          (vimp-contract-range newrange))
        newrange))))

(defmacro vimp-define-text-object (object args &rest body)
  "Define a text object command OBJECT.
BODY should return a range (BEG END) to the right of point
if COUNT is positive, and to the left of it if negative.

\(fn OBJECT (COUNT) DOC [[KEY VALUE]...] BODY...)"
  (declare (indent defun)
           (debug (&define name lambda-list
                           [&optional stringp]
                           [&rest keywordp sexp]
                           def-body)))
  (let* ((args (delq '&optional args))
         (count (or (pop args) 'count))
         (args (when args `(&optional ,@args)))
         (interactive '((interactive "<c><v>")))
         arg doc key keys)
    ;; collect docstring
    (when (stringp (car-safe body))
      (setq doc (pop body)))
    ;; collect keywords
    (setq keys (plist-put keys :extend-selection t))
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body)
            keys (plist-put keys key arg)))
    ;; interactive
    (when (eq (car-safe (car-safe body)) 'interactive)
      (setq interactive (list (pop body))))
    ;; macro expansion
    `(vimp-define-motion ,object (,count ,@args)
       ,@(when doc `(,doc))
       ,@keys
       ,@interactive
       (setq ,count (or ,count 1))
       (when (/= ,count 0)
         (let ((type (vimp-type ',object vimp-visual-char))
               (extend (and (vimp-visual-state-p)
                            (vimp-get-command-property
                             ',object :extend-selection
                             ',(plist-get keys :extend-selection))))
               (dir vimp-visual-direction)
               mark point range selection)
           (cond
            ;; Visual state: extend the current selection
            ((and (vimp-visual-state-p)
                  (vimp-called-interactively-p))
             ;; if we are at the beginning of the Visual selection,
             ;; go to the left (negative COUNT); if at the end,
             ;; go to the right (positive COUNT)
             (setq dir vimp-visual-direction
                   ,count (* ,count dir))
             (setq range (progn ,@body))
             (when (vimp-range-p range)
               (setq range (vimp-expand-range range))
               (vimp-set-type range (vimp-type range type))
               (setq range (vimp-contract-range range))
               ;; the beginning is mark and the end is point
               ;; unless the selection goes the other way
               (setq mark  (vimp-range-beginning range)
                     point (vimp-range-end range)
                     type  (vimp-type
                            (if vimp-text-object-change-visual-type
                                range
                              (vimp-visual-range))))
               (when (and (eq type 'line)
                          (not (eq type (vimp-type range))))
                 (let ((newrange (vimp-text-object-make-linewise range)))
                   (setq mark (vimp-range-beginning newrange)
                         point (vimp-range-end newrange))))
               (when (< dir 0)
                 (vimp-swap mark point))
               ;; select the union
               (vimp-visual-make-selection mark point type)))
            ;; not Visual state: return a pair of buffer positions
            (t
             (setq range (progn ,@body))
             (unless (vimp-range-p range)
               (setq ,count (- ,count)
                     range (progn ,@body)))
             (when (vimp-range-p range)
               (setq selection (vimp-range (point) (point) type))
               (if extend
                   (setq range (vimp-range-union range selection))
                 (vimp-set-type range (vimp-type range type)))
               ;; ensure the range is properly expanded
               (vimp-contract-range range)
               (vimp-expand-range range)
               ;; possibly convert to linewise
               (when (eq vimp-this-type-modified 'line)
                 (setq range (vimp-text-object-make-linewise range)))
               (vimp-set-range-properties range nil)
               range))))))))

(defmacro vimp-define-operator (operator args &rest body)
  "Define an operator command OPERATOR.

\(fn OPERATOR (BEG END ARGS...) DOC [[KEY VALUE]...] BODY...)"
  (declare (indent defun)
           (debug (&define name lambda-list
                           [&optional stringp]
                           [&rest keywordp sexp]
                           [&optional ("interactive" [&rest form])]
                           def-body)))
  (let* ((args (delq '&optional args))
         (interactive (if (> (length args) 2) '("<R>") '("<r>")))
         (args (if (> (length args) 2)
                   `(,(nth 0 args) ,(nth 1 args)
                     &optional ,@(nthcdr 2 args))
                 args))
         arg doc key keys visual)
    ;; collect docstring
    (when (and (> (length body) 1)
               (or (eq (car-safe (car-safe body)) 'format)
                   (stringp (car-safe body))))
      (setq doc (pop body)))
    ;; collect keywords
    (setq keys (plist-put keys :move-point t))
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body))
      (cond
       ((eq key :keep-visual)
        (setq visual arg))
       (t
        (setq keys (plist-put keys key arg)))))
    ;; collect `interactive' specification
    (when (eq (car-safe (car-safe body)) 'interactive)
      (setq interactive (cdr-safe (pop body))))
    ;; transform extended interactive specs
    (setq interactive (apply #'vimp-interactive-form interactive))
    (setq keys (vimp-concat-plists keys (cdr-safe interactive))
          interactive (car-safe interactive))
    ;; macro expansion
    `(vimp-define-command ,operator ,args
       ,@(when doc `(,doc))
       ,@keys
       :keep-visual t
       :suppress-operator t
       (interactive
        (let* ((vimp-operator-range-motion
                (when (vimp-has-command-property-p ',operator :motion)
                  ;; :motion nil is equivalent to :motion undefined
                  (or (vimp-get-command-property ',operator :motion)
                      #'undefined)))
               (vimp-operator-range-type
                (vimp-get-command-property ',operator :type))
               (orig (point))
               vimp-operator-range-beginning
               vimp-operator-range-end
               vimp-inhibit-operator)
          (setq vimp-inhibit-operator-value nil
                vimp-this-operator this-command)
          (prog1 ,interactive
            (setq orig (point)
                  vimp-inhibit-operator-value vimp-inhibit-operator)
            (if ,visual
                (when (vimp-visual-state-p)
                  (vimp-visual-expand-region))
              (when (or (vimp-visual-state-p) (region-active-p))
                (setq deactivate-mark t)))
            (cond
             ((vimp-visual-state-p)
              (vimp-visual-rotate 'upper-left))
             ((vimp-get-command-property ',operator :move-point)
              (goto-char (or vimp-operator-range-beginning orig)))
             (t
              (goto-char orig))))))
       (unwind-protect
           (let ((vimp-inhibit-operator vimp-inhibit-operator-value))
             (unless (and vimp-inhibit-operator
                          (vimp-called-interactively-p))
               ,@body))
         (setq vimp-inhibit-operator-value nil)))))

;; this is used in the `interactive' specification of an operator command
(defun vimp-operator-range (&optional return-type)
  "Read a motion from the keyboard and return its buffer positions.
The return value is a list (BEG END), or (BEG END TYPE) if
RETURN-TYPE is non-nil."
  (let ((motion (or vimp-operator-range-motion
                    (when (vimp-ex-p) 'vimp-line)))
        (type vimp-operator-range-type)
        (range (vimp-range (point) (point)))
        command count modifier)
    (setq vimp-this-type-modified nil)
    (vimp-save-echo-area
      (cond
       ;; Ex mode
       ((and (vimp-ex-p) vimp-ex-range)
        (setq range vimp-ex-range))
       ;; Visual selection
       ((and (not (vimp-ex-p)) (vimp-visual-state-p))
        (setq range (vimp-visual-range)))
       ;; active region
       ((and (not (vimp-ex-p)) (region-active-p))
        (setq range (vimp-range (region-beginning)
                                (region-end)
                                (or vimp-this-type 'exclusive))))
       (t
        ;; motion
        (vimp-save-state
          (unless motion
            (vimp-change-state 'operator)
            ;; Make linewise operator shortcuts. E.g., "d" yields the
            ;; shortcut "dd", and "g?" yields shortcuts "g??" and "g?g?".
            (let ((keys (nth 2 (vimp-extract-count (this-command-keys)))))
              (setq keys (listify-key-sequence keys))
              (dotimes (var (length keys))
                (define-key vimp-operator-shortcut-map
                  (vconcat (nthcdr var keys)) 'vimp-line)))
            ;; read motion from keyboard
            (setq command (vimp-read-motion motion)
                  motion (nth 0 command)
                  count (nth 1 command)
                  type (or type (nth 2 command))))
          (cond
           ((eq motion #'undefined)
            (setq range (if return-type '(nil nil nil) '(nil nil))
                  motion nil))
           ((or (null motion) ; keyboard-quit
                (vimp-get-command-property motion :suppress-operator))
            (when (fboundp 'vimp-repeat-abort)
              (vimp-repeat-abort))
            (setq quit-flag t
                  motion nil))
           (vimp-repeat-count
            (setq count vimp-repeat-count
                  ;; only the first operator's count is overwritten
                  vimp-repeat-count nil))
           ((or count current-prefix-arg)
            ;; multiply operator count and motion count together
            (setq count
                  (* (prefix-numeric-value count)
                     (prefix-numeric-value current-prefix-arg)))))
          (when motion
            (let ((vimp-state 'operator)
                  mark-active)
              ;; calculate motion range
              (setq range (vimp-motion-range
                           motion
                           count
                           type))))
          ;; update global variables
          (setq vimp-this-motion motion
                vimp-this-motion-count count
                type (vimp-type range type)
                vimp-this-type type))))
      (when (vimp-range-p range)
        (unless (or (null type) (eq (vimp-type range) type))
          (vimp-contract-range range)
          (vimp-set-type range type)
          (vimp-expand-range range))
        (vimp-set-range-properties range nil)
        (unless return-type
          (vimp-set-type range nil))
        (setq vimp-operator-range-beginning (vimp-range-beginning range)
              vimp-operator-range-end (vimp-range-end range)
              vimp-operator-range-type (vimp-type range)))
      range)))

(defmacro vimp-define-type (type doc &rest body)
  "Define type TYPE.
DOC is a general description and shows up in all docstrings.
It is followed by a list of keywords and functions:

:expand FUNC     Expansion function. This function should accept
                 two positions in the current buffer, BEG and END,
                 and return a pair of expanded buffer positions.
:contract FUNC   The opposite of :expand, optional.
:one-to-one BOOL Whether expansion is one-to-one. This means that
                 :expand followed by :contract always returns the
                 original range.
:normalize FUNC  Normalization function, optional. This function should
                 accept two unexpanded positions and adjust them before
                 expansion. May be used to deal with buffer boundaries.
:string FUNC     Description function. This takes two buffer positions
                 and returns a human-readable string, for example,
                 \"2 lines\".

If further keywords and functions are specified, they are assumed to
be transformations on buffer positions, like :expand and :contract.

\(fn TYPE DOC [[KEY FUNC]...])"
  (declare (indent defun)
           (debug (&define name
                           [&optional stringp]
                           [&rest [keywordp function-form]])))
  (let (args defun-forms func key name plist string sym val)
    ;; standard values
    (setq plist (plist-put plist :one-to-one t))
    ;; keywords
    (while (keywordp (car-safe body))
      (setq key (pop body)
            val (pop body))
      (if (plist-member plist key) ; not a function
          (setq plist (plist-put plist key val))
        (setq func val
              sym (intern (replace-regexp-in-string
                           "^:" "" (symbol-name key)))
              name (intern (format "vimp-%s-%s" type sym))
              args (car (cdr-safe func))
              string (car (cdr (cdr-safe func)))
              string (if (stringp string)
                         (format "%s\n\n" string) "")
              plist (plist-put plist key `',name))
        (add-to-list
         'defun-forms
         (cond
          ((eq key :string)
           `(defun ,name (beg end &rest properties)
              ,(format "Return size of %s from BEG to END \
with PROPERTIES.\n\n%s%s" type string doc)
              (let ((beg (vimp-normalize-position beg))
                    (end (vimp-normalize-position end))
                    (type ',type)
                    plist range)
                (when (and beg end)
                  (save-excursion
                    (vimp-sort beg end)
                    (unless (plist-get properties :expanded)
                      (setq range (apply #'vimp-expand
                                         beg end type properties)
                            beg (vimp-range-beginning range)
                            end (vimp-range-end range)
                            type (vimp-type range type)
                            plist (vimp-range-properties range))
                      (setq properties
                            (vimp-concat-plists properties plist)))
                    (or (apply #',func beg end
                               (when ,(> (length args) 2)
                                 properties))
                        ""))))))
          (t
           `(defun ,name (beg end &rest properties)
              ,(format "Perform %s transformation on %s from BEG to END \
with PROPERTIES.\n\n%s%s" sym type string doc)
              (let ((beg (vimp-normalize-position beg))
                    (end (vimp-normalize-position end))
                    (type ',type)
                    plist range)
                (when (and beg end)
                  (save-excursion
                    (vimp-sort beg end)
                    (when (memq ,key '(:expand :contract))
                      (setq properties
                            (plist-put properties
                                       :expanded
                                       ,(eq key :expand))))
                    (setq range (or (apply #',func beg end
                                           (when ,(> (length args) 2)
                                             properties))
                                    (apply #'vimp-range
                                           beg end type properties))
                          beg (vimp-range-beginning range)
                          end (vimp-range-end range)
                          type (vimp-type range type)
                          plist (vimp-range-properties range))
                    (setq properties
                          (vimp-concat-plists properties plist))
                    (apply #'vimp-range beg end type properties)))))))
         t)))
    ;; :one-to-one requires both or neither of :expand and :contract
    (when (plist-get plist :expand)
      (setq plist (plist-put plist :one-to-one
                             (and (plist-get plist :contract)
                                  (plist-get plist :one-to-one)))))
    `(progn
       (vimp-put-property 'vimp-type-properties ',type ,@plist)
       ,@defun-forms
       ',type)))

(defmacro vimp-define-interactive-code (code &rest body)
  "Define an interactive code.
PROMPT, if given, is the remainder of the interactive string
up to the next newline. Command properties may be specified
via KEY-VALUE pairs. BODY should evaluate to a list of values.

\(fn CODE (PROMPT) [[KEY VALUE]...] BODY...)"
  (declare (indent defun))
  (let* ((args (when (and (> (length body) 1)
                          (listp (car-safe body)))
                 (pop body)))
         (doc (when (stringp (car-safe body)) (pop body)))
         func properties)
    (while (keywordp (car-safe body))
      (setq properties
            (append properties (list (pop body) (pop body)))))
    (cond
     (args
      (setq func `(lambda ,args
                    ,@(when doc `(,doc))
                    ,@body)))
     ((> (length body) 1)
      (setq func `(progn ,@body)))
     (t
      (setq func (car body))))
    `(eval-and-compile
       (let* ((code ,code)
              (entry (assoc code vimp-interactive-alist))
              (value (cons ',func ',properties)))
         (if entry
             (setcdr entry value)
           (push (cons code value) vimp-interactive-alist))
         code))))

;;; Highlighting

(when (fboundp 'font-lock-add-keywords)
  (font-lock-add-keywords
   'emacs-lisp-mode
   ;; Match all `vimp-define-' forms except `vimp-define-key'.
   ;; (In the interests of speed, this expression is incomplete
   ;; and does not match all three-letter words.)
   '(("(\\(vimp-\\(?:ex-\\)?define-\
\\(?:[^ k][^ e][^ y]\\|[-[:word:]]\\{4,\\}\\)\\)\
\\>[ \f\t\n\r\v]*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face nil t))
     ("(\\(vimp-\\(?:delay\\|narrow\\|signal\\|save\\|with\\(?:out\\)?\\)\
\\(?:-[-[:word:]]+\\)?\\)\\>\[ \f\t\n\r\v]+"
      1 font-lock-keyword-face)
     ("(\\(vimp-\\(?:[-[:word:]]\\)*loop\\)\\>[ \f\t\n\r\v]+"
      1 font-lock-keyword-face))))

(provide 'vimp-macros)

;;; vimp-macros.el ends here
