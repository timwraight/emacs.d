;;; vimp-commands.el --- Evil commands and operators
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

(require 'vimp-common)
(require 'vimp-digraphs)
(require 'vimp-search)
(require 'vimp-ex)
(require 'vimp-types)
(require 'vimp-command-window)

;;; Compatibility for Emacs 23
(unless (fboundp 'window-body-width)
  (defalias 'window-body-width 'window-width))

;;; Motions

;; Movement commands, or motions, are defined with the macro
;; `vimp-define-motion'. A motion is a command with an optional
;; argument COUNT (interactively accessed by the code "<c>").
;; It may specify the :type command property (e.g., :type line),
;; which determines how it is handled by an operator command.
;; Furthermore, the command must have the command properties
;; :keep-visual t and :repeat motion; these are automatically
;; set by the `vimp-define-motion' macro.

;;; Code:

(vimp-define-motion vimp-forward-char (count &optional crosslines noerror)
  "Move cursor to the right by COUNT characters.
Movement is restricted to the current line unless CROSSLINES is non-nil.
If NOERROR is non-nil, don't signal an error upon reaching the end
of the line or the buffer; just return nil."
  :type exclusive
  (interactive "<c>" (list vimp-cross-lines
                           (vimp-kbd-macro-suppress-motion-error)))
  (cond
   (noerror
    (condition-case nil
        (vimp-forward-char count crosslines nil)
      (error nil)))
   ((not crosslines)
    ;; for efficiency, narrow the buffer to the projected
    ;; movement before determining the current line
    (vimp-with-restriction
        (point)
        (save-excursion
          (vimp-forward-char (1+ (or count 1)) t t)
          (point))
      (vimp-narrow-to-line
        (vimp-forward-char count t noerror))))
   (t
    (vimp-motion-loop (nil (or count 1))
      (forward-char)
      ;; don't put the cursor on a newline
      (when (and vimp-move-cursor-back
                 (not vimp-move-beyond-eol)
                 (not (vimp-visual-state-p))
                 (not (vimp-operator-state-p))
                 (eolp) (not (eobp)) (not (bolp)))
        (forward-char))))))

(vimp-define-motion vimp-backward-char (count &optional crosslines noerror)
  "Move cursor to the left by COUNT characters.
Movement is restricted to the current line unless CROSSLINES is non-nil.
If NOERROR is non-nil, don't signal an error upon reaching the beginning
of the line or the buffer; just return nil."
  :type exclusive
  (interactive "<c>" (list vimp-cross-lines
                           (vimp-kbd-macro-suppress-motion-error)))
  (cond
   (noerror
    (condition-case nil
        (vimp-backward-char count crosslines nil)
      (error nil)))
   ((not crosslines)
    ;; restrict movement to the current line
    (vimp-with-restriction
        (save-excursion
          (vimp-backward-char (1+ (or count 1)) t t)
          (point))
        (1+ (point))
      (vimp-narrow-to-line
        (vimp-backward-char count t noerror))))
   (t
    (vimp-motion-loop (nil (or count 1))
      (backward-char)
      ;; don't put the cursor on a newline
      (unless (or (vimp-visual-state-p) (vimp-operator-state-p))
        (vimp-adjust-cursor))))))

(vimp-define-motion vimp-next-line (count)
  "Move the cursor COUNT lines down."
  :type line
  (let (line-move-visual)
    (vimp-line-move (or count 1))))

(vimp-define-motion vimp-previous-line (count)
  "Move the cursor COUNT lines up."
  :type line
  (let (line-move-visual)
    (vimp-line-move (- (or count 1)))))

(vimp-define-motion vimp-next-visual-line (count)
  "Move the cursor COUNT screen lines down."
  :type exclusive
  (let ((line-move-visual t))
    (vimp-line-move (or count 1))))

(vimp-define-motion vimp-previous-visual-line (count)
  "Move the cursor COUNT screen lines up."
  :type exclusive
  (let ((line-move-visual t))
    (vimp-line-move (- (or count 1)))))

;; used for repeated commands like "dd"
(vimp-define-motion vimp-line (count)
  "Move COUNT - 1 lines down."
  :type line
  (let (line-move-visual)
    ;; Catch bob and eob errors. These are caused when not moving
    ;; point starting in the first or last line, respectively. In this
    ;; case the current line should be selected.
    (condition-case err
        (vimp-line-move (1- (or count 1)))
      ((beginning-of-buffer end-of-buffer)))))

(vimp-define-motion vimp-beginning-of-line ()
  "Move the cursor to the beginning of the current line."
  :type exclusive
  (move-beginning-of-line nil))

(vimp-define-motion vimp-end-of-line (count)
  "Move the cursor to the end of the current line.
If COUNT is given, move COUNT - 1 lines downward first."
  :type inclusive
  (move-end-of-line count)
  (when vimp-track-eol
    (setq temporary-goal-column most-positive-fixnum
          this-command 'next-line))
  (unless (vimp-visual-state-p)
    (vimp-adjust-cursor)
    (when (eolp)
      ;; prevent "c$" and "d$" from deleting blank lines
      (setq vimp-this-type 'exclusive))))

(vimp-define-motion vimp-beginning-of-visual-line ()
  "Move the cursor to the first character of the current screen line."
  :type exclusive
  (if (fboundp 'beginning-of-visual-line)
      (beginning-of-visual-line)
    (beginning-of-line)))

(vimp-define-motion vimp-end-of-visual-line (count)
  "Move the cursor to the last character of the current screen line.
If COUNT is given, move COUNT - 1 screen lines downward first."
  :type inclusive
  (if (fboundp 'end-of-visual-line)
      (end-of-visual-line count)
    (end-of-line count)))

(vimp-define-motion vimp-middle-of-visual-line ()
  "Move the cursor to the middle of the current visual line."
  :type exclusive
  (beginning-of-visual-line)
  (vimp-with-restriction
      nil
      (save-excursion (end-of-visual-line) (point))
    (move-to-column (+ (current-column)
                       -1
                       (/ (with-no-warnings (window-body-width)) 2)))))

(vimp-define-motion vimp-beginning-of-line-or-digit-argument ()
  "Move the cursor to the beginning of the current line.
This function passes its command to `digit-argument' (usually a 0)
if it is not the first event."
  :type exclusive
  (cond
   (current-prefix-arg
    (setq this-command #'digit-argument)
    (call-interactively #'digit-argument))
   (t
    (setq this-command #'vimp-beginning-of-line)
    (call-interactively #'vimp-beginning-of-line))))

(vimp-define-motion vimp-first-non-blank ()
  "Move the cursor to the first non-blank character of the current line."
  :type exclusive
  (vimp-narrow-to-line (back-to-indentation)))

(vimp-define-motion vimp-last-non-blank (count)
  "Move the cursor to the last non-blank character of the current line.
If COUNT is given, move COUNT - 1 lines downward first."
  :type inclusive
  (goto-char
   (save-excursion
     (vimp-move-beginning-of-line count)
     (if (re-search-forward "[ \t]*$")
         (max (line-beginning-position)
              (1- (match-beginning 0)))
       (line-beginning-position)))))

(vimp-define-motion vimp-first-non-blank-of-visual-line ()
  "Move the cursor to the first non blank character
of the current screen line."
  :type exclusive
  (vimp-beginning-of-visual-line)
  (skip-chars-forward " \t\r"))

(vimp-define-motion vimp-next-line-first-non-blank (count)
  "Move the cursor COUNT lines down on the first non-blank character."
  :type line
  (vimp-next-line (or count 1))
  (vimp-first-non-blank))

(vimp-define-motion vimp-next-line-1-first-non-blank (count)
  "Move the cursor COUNT-1 lines down on the first non-blank character."
  :type line
  (vimp-next-line (1- (or count 1)))
  (vimp-first-non-blank))

(vimp-define-motion vimp-previous-line-first-non-blank (count)
  "Move the cursor COUNT lines up on the first non-blank character."
  :type line
  (vimp-previous-line (or count 1))
  (vimp-first-non-blank))

(vimp-define-motion vimp-goto-line (count)
  "Go to the first non-blank character of line COUNT.
By default the last line."
  :jump t
  :type line
  (if (null count)
      (goto-char (point-max))
    (goto-char (point-min))
    (forward-line (1- count)))
  (vimp-first-non-blank))

(vimp-define-motion vimp-goto-first-line (count)
  "Go to the first non-blank character of line COUNT.
By default the first line."
  :jump t
  :type line
  (vimp-goto-line (or count 1)))

(vimp-define-motion vimp-forward-word-begin (count &optional bigword)
  "Move the cursor to the beginning of the COUNT-th next word.
If BIGWORD is non-nil, move by WORDS.

If this command is called in operator-pending state it behaves
differently. If point reaches the beginning of a word on a new
line point is moved back to the end of the previous line.

If called after a change operator, i.e. cw or cW,
`vimp-want-change-word-to-end' is non-nil and point is on a word,
then both behave like ce or cE.

If point is at the end of the buffer and cannot be moved signal
'end-of-buffer is raised.
"
  :type exclusive
  (let ((thing (if bigword 'vimp-WORD 'vimp-word))
        (orig (point))
        (count (or count 1)))
    (vimp-signal-at-bob-or-eob count)
    (cond
     ;; default motion, beginning of next word
     ((not (vimp-operator-state-p))
      (vimp-forward-beginning thing count))
     ;; the vimp-change operator, maybe behave like ce or cE
     ((and vimp-want-change-word-to-end
           (eq vimp-this-operator #'vimp-change)
           (< orig (or (cdr-safe (bounds-of-thing-at-point thing)) orig)))
      ;; forward-thing moves point to the correct position because
      ;; this is an exclusive motion
      (forward-thing thing count))
     ;; operator state
     (t
      (prog1 (vimp-forward-beginning thing count)
        ;; if we reached the beginning of a word on a new line in
        ;; Operator-Pending state, go back to the end of the previous
        ;; line
        (when (and (> (line-beginning-position) orig)
                   (looking-back "^[[:space:]]*" (line-beginning-position)))
          ;; move cursor back as long as the line contains only
          ;; whitespaces and is non-empty
          (vimp-move-end-of-line 0)
          ;; skip non-empty lines containing only spaces
          (while (and (looking-back "^[[:space:]]+$" (line-beginning-position))
                      (not (<= (line-beginning-position) orig)))
            (vimp-move-end-of-line 0))
          ;; but if the previous line is empty, delete this line
          (when (bolp) (forward-char))))))))

(vimp-define-motion vimp-forward-word-end (count &optional bigword)
  "Move the cursor to the end of the COUNT-th next word.
If BIGWORD is non-nil, move by WORDS."
  :type inclusive
  (let ((thing (if bigword 'vimp-WORD 'vimp-word))
        (count (or count 1)))
    (vimp-signal-at-bob-or-eob count)
    ;; Evil special behaviour: e or E on a one-character word in
    ;; operator state does not move point
    (unless (and (vimp-operator-state-p)
                 (= 1 count)
                 (let ((bnd (bounds-of-thing-at-point thing)))
                   (and bnd
                        (= (car bnd) (point))
                        (= (cdr bnd) (1+ (point)))))
                 (looking-at "[[:word:]]"))
      (vimp-forward-end thing count))))

(vimp-define-motion vimp-backward-word-begin (count &optional bigword)
  "Move the cursor to the beginning of the COUNT-th previous word.
If BIGWORD is non-nil, move by WORDS."
  :type exclusive
  (let ((thing (if bigword 'vimp-WORD 'vimp-word)))
    (vimp-signal-at-bob-or-eob (- (or count 1)))
    (vimp-backward-beginning thing count)))

(vimp-define-motion vimp-backward-word-end (count &optional bigword)
  "Move the cursor to the end of the COUNT-th previous word.
If BIGWORD is non-nil, move by WORDS."
  :type inclusive
  (let ((thing (if bigword 'vimp-WORD 'vimp-word)))
    (vimp-signal-at-bob-or-eob (- (or count 1)))
    (vimp-backward-end thing count)))

(vimp-define-motion vimp-forward-WORD-begin (count)
  "Move the cursor to the beginning of the COUNT-th next WORD."
  :type exclusive
  (vimp-forward-word-begin count t))

(vimp-define-motion vimp-forward-WORD-end (count)
  "Move the cursor to the end of the COUNT-th next WORD."
  :type inclusive
  (vimp-forward-word-end count t))

(vimp-define-motion vimp-backward-WORD-begin (count)
  "Move the cursor to the beginning of the COUNT-th previous WORD."
  :type exclusive
  (vimp-backward-word-begin count t))

(vimp-define-motion vimp-backward-WORD-end (count)
  "Move the cursor to the end of the COUNT-th previous WORD."
  :type inclusive
  (vimp-backward-word-end count t))

;; section movement
(vimp-define-motion vimp-forward-section-begin (count)
  "Move the cursor to the beginning of the COUNT-th next section."
  :jump t
  :type exclusive
  (vimp-signal-at-bob-or-eob count)
  (vimp-forward-beginning 'vimp-defun count))

(vimp-define-motion vimp-forward-section-end (count)
  "Move the cursor to the end of the COUNT-th next section."
  :jump t
  :type inclusive
  (vimp-signal-at-bob-or-eob count)
  (vimp-forward-end 'vimp-defun count)
  (unless (eobp) (forward-line)))

(vimp-define-motion vimp-backward-section-begin (count)
  "Move the cursor to the beginning of the COUNT-th previous section."
  :jump t
  :type exclusive
  (vimp-signal-at-bob-or-eob (- (or count 1)))
  (vimp-backward-beginning 'vimp-defun count))

(vimp-define-motion vimp-backward-section-end (count)
  "Move the cursor to the end of the COUNT-th previous section."
  :jump t
  :type inclusive
  (vimp-signal-at-bob-or-eob (- (or count 1)))
  (end-of-line -1)
  (vimp-backward-end 'vimp-defun count)
  (unless (eobp) (forward-line)))

(vimp-define-motion vimp-forward-sentence-begin (count)
  "Move to the next COUNT-th beginning of a sentence or end of a paragraph."
  :jump t
  :type exclusive
  (vimp-signal-at-bob-or-eob count)
  (vimp-forward-nearest count
                        #'(lambda (cnt)
                            (vimp-forward-beginning 'vimp-sentence))
                        #'vimp-forward-paragraph))

(vimp-define-motion vimp-backward-sentence-begin (count)
  "Move to the previous COUNT-th beginning of a sentence or paragraph."
  :jump t
  :type exclusive
  (vimp-signal-at-bob-or-eob (- (or count 1)))
  (vimp-forward-nearest (- (or count 1))
                        #'(lambda (cnt)
                            (vimp-backward-beginning 'vimp-sentence))
                        #'(lambda (cnt)
                            (vimp-backward-paragraph))))

(vimp-define-motion vimp-forward-paragraph (count)
  "Move to the end of the COUNT-th next paragraph."
  :jump t
  :type exclusive
  (vimp-signal-at-bob-or-eob count)
  (vimp-forward-end 'vimp-paragraph count)
  (unless (eobp) (forward-line)))

(vimp-define-motion vimp-backward-paragraph (count)
  "Move to the beginning of the COUNT-th previous paragraph."
  :jump t
  :type exclusive
  (vimp-signal-at-bob-or-eob (- (or count 1)))
  (unless (eobp) (forward-line))
  (vimp-backward-beginning 'vimp-paragraph count)
  (unless (bobp) (forward-line -1)))

(vimp-define-motion vimp-jump-item (count)
  "Find the next item in this line after or under the cursor
and jump to the corresponding one."
  :jump t
  :type inclusive
  (cond
   ;; COUNT% jumps to a line COUNT percentage down the file
   (count
    (goto-char
     (vimp-normalize-position
      (let ((size (- (point-max) (point-min))))
        (+ (point-min)
           (if (> size 80000)
               (* count (/ size 100))
             (/ (* count size) 100))))))
    (back-to-indentation)
    (setq vimp-this-type 'line))
   ((and (vimp-looking-at-start-comment t)
         (let ((pnt (point)))
           (forward-comment 1)
           (or (not (bolp))
               (prog1 nil (goto-char pnt)))))
    (backward-char))
   ((and (not (eolp)) (vimp-looking-at-end-comment t))
    (forward-comment -1))
   ((and
     (memq major-mode '(c-mode c++-mode))
     (require 'hideif nil t)
     (with-no-warnings
       (let* ((hif-else-regexp (concat hif-cpp-prefix "\\(?:else\\|elif[ \t]+\\)"))
              (hif-ifx-else-endif-regexp
               (concat hif-ifx-regexp "\\|" hif-else-regexp "\\|" hif-endif-regexp)))
         (cond
          ((save-excursion (beginning-of-line) (or (hif-looking-at-ifX) (hif-looking-at-else)))
           (hif-find-next-relevant)
           (while (hif-looking-at-ifX)
             (hif-ifdef-to-endif)
             (hif-find-next-relevant))
           t)
          ((save-excursion (beginning-of-line) (hif-looking-at-endif))
           (hif-endif-to-ifdef)
           t))))))
   (t
    (let* ((open (point-max))
           (close (point-max))
           (open-pair (condition-case nil
                          (save-excursion
                            ;; consider the character right before eol given that
                            ;; point may be placed there, e.g. in visual state
                            (when (and (eolp) (not (bolp)))
                              (backward-char))
                            (setq open (1- (scan-lists (point) 1 -1)))
                            (when (< open (line-end-position))
                              (goto-char open)
                              (forward-list)
                              (1- (point))))
                        (error nil)))
           (close-pair (condition-case nil
                           (save-excursion
                             ;; consider the character right before eol given that
                             ;; point may be placed there, e.g. in visual state
                             (when (and (eolp) (not (bolp)))
                               (backward-char))
                             (setq close (1- (scan-lists (point) 1 1)))
                             (when (< close (line-end-position))
                               (goto-char (1+ close))
                               (backward-list)
                               (point)))
                         (error nil))))
      (cond
       ((not (or open-pair close-pair))
        ;; nothing found, check if we are inside a string
        (let ((pnt (point))
              (state (syntax-ppss (point)))
              (bnd (bounds-of-thing-at-point 'vimp-string)))
          (if (not (and bnd (< (point) (cdr bnd))))
              ;; no, then we really failed
              (user-error "No matching item found on the current line")
            ;; yes, go to the end of the string and try again
            (let ((endstr (cdr bnd)))
              (when (or (save-excursion
                          (goto-char endstr)
                          (let ((b (bounds-of-thing-at-point 'vimp-string)))
                            (and b (< (point) (cdr b))))) ; not at end of string
                        (condition-case nil
                            (progn
                              (goto-char endstr)
                              (vimp-jump-item)
                              nil)
                          (error t)))
                ;; failed again, go back to original point
                (goto-char pnt)
                (user-error "No matching item found on the current line"))))))
       ((< open close) (goto-char open-pair))
       (t (goto-char close-pair)))))))

(vimp-define-motion vimp-previous-open-paren (count)
  "Go to [count] previous unmatched '('."
  :type exclusive
  (vimp-up-paren ?( ?) (- (or count 1))))

(vimp-define-motion vimp-next-close-paren (count)
  "Go to [count] next unmatched ')'."
  :type exclusive
  (forward-char)
  (vimp-up-paren ?( ?) (or count 1))
  (backward-char))

(vimp-define-motion vimp-previous-open-brace (count)
  "Go to [count] previous unmatched '{'."
  :type exclusive
  (vimp-up-paren ?{ ?} (- (or count 1))))

(vimp-define-motion vimp-next-close-brace (count)
  "Go to [count] next unmatched '}'."
  :type exclusive
  (forward-char)
  (vimp-up-paren ?{ ?} (or count 1))
  (backward-char))

(vimp-define-motion vimp-find-char (count char)
  "Move to the next COUNT'th occurrence of CHAR."
  :jump t
  :type inclusive
  (interactive "<c><C>")
  (setq count (or count 1))
  (let ((fwd (> count 0)))
    (setq vimp-last-find (list #'vimp-find-char char fwd))
    (when fwd (forward-char))
    (let ((case-fold-search nil))
      (unless (prog1
                  (search-forward (char-to-string char)
                                  (unless vimp-cross-lines
                                    (if fwd
                                        (line-end-position)
                                      (line-beginning-position)))
                                  t count)
                (when fwd (backward-char)))
        (user-error "Can't find %c" char)))))

(vimp-define-motion vimp-find-char-backward (count char)
  "Move to the previous COUNT'th occurrence of CHAR."
  :jump t
  :type exclusive
  (interactive "<c><C>")
  (vimp-find-char (- (or count 1)) char))

(vimp-define-motion vimp-find-char-to (count char)
  "Move before the next COUNT'th occurrence of CHAR."
  :jump t
  :type inclusive
  (interactive "<c><C>")
  (unwind-protect
      (progn
        (vimp-find-char count char)
        (if (> (or count 1) 0)
            (backward-char)
          (forward-char)))
    (setcar vimp-last-find #'vimp-find-char-to)))

(vimp-define-motion vimp-find-char-to-backward (count char)
  "Move before the previous COUNT'th occurrence of CHAR."
  :jump t
  :type exclusive
  (interactive "<c><C>")
  (vimp-find-char-to (- (or count 1)) char))

(vimp-define-motion vimp-repeat-find-char (count)
  "Repeat the last find COUNT times."
  :jump t
  :type inclusive
  (setq count (or count 1))
  (if vimp-last-find
      (let ((cmd (car vimp-last-find))
            (char (nth 1 vimp-last-find))
            (fwd (nth 2 vimp-last-find))
            vimp-last-find)
        ;; ensure count is non-negative
        (when (< count 0)
          (setq count (- count)
                fwd (not fwd)))
        ;; skip next character when repeating t or T
        (and (eq cmd #'vimp-find-char-to)
             vimp-repeat-find-to-skip-next
             (= count 1)
             (or (and fwd (= (char-after (1+ (point))) char))
                 (and (not fwd) (= (char-before) char)))
             (setq count (1+ count)))
        (funcall cmd (if fwd count (- count)) char)
        (unless (nth 2 vimp-last-find)
          (setq vimp-this-type 'exclusive)))
    (user-error "No previous search")))

(vimp-define-motion vimp-repeat-find-char-reverse (count)
  "Repeat the last find COUNT times in the opposite direction."
  :jump t
  :type inclusive
  (vimp-repeat-find-char (- (or count 1))))

;; ceci n'est pas une pipe
(vimp-define-motion vimp-goto-column (count)
  "Go to column COUNT on the current line.
Columns are counted from zero."
  :type exclusive
  (move-to-column (or count 0)))

(vimp-define-command vimp-goto-mark (char &optional noerror)
  "Go to the marker specified by CHAR."
  :keep-visual t
  :repeat nil
  :type exclusive
  (interactive (list (read-char)))
  (let ((marker (vimp-get-marker char)))
    (cond
     ((markerp marker)
      (switch-to-buffer (marker-buffer marker))
      (goto-char (marker-position marker)))
     ((numberp marker)
      (goto-char marker))
     ((consp marker)
      (when (or (find-buffer-visiting (car marker))
                (and (y-or-n-p (format "Visit file %s again? "
                                       (car marker)))
                     (find-file (car marker))))
        (goto-char (cdr marker))))
     ((not noerror)
      (user-error "Marker `%c' is not set%s" char
                  (if (vimp-global-marker-p char) ""
                    " in this buffer"))))))

(vimp-define-command vimp-goto-mark-line (char &optional noerror)
  "Go to the line of the marker specified by CHAR."
  :keep-visual t
  :repeat nil
  :type line
  (interactive (list (read-char)))
  (vimp-goto-mark char noerror)
  (vimp-first-non-blank))

(vimp-define-motion vimp-jump-backward (count)
  "Go to older position in jump list.
To go the other way, press \
\\<vimp-motion-state-map>\\[vimp-jump-forward]."
  (let ((current-pos (make-marker))
        (count (or count 1)) i)
    (unless vimp-jump-list
      (move-marker current-pos (point))
      (add-to-list 'vimp-jump-list current-pos))
    (vimp-motion-loop (nil count)
      (setq current-pos (make-marker))
      ;; skip past duplicate entries in the mark ring
      (setq i (length mark-ring))
      (while (progn (move-marker current-pos (point))
                    (set-mark-command 0)
                    (setq i (1- i))
                    (and (= (point) current-pos) (> i 0))))
      ;; Already there?
      (move-marker current-pos (point))
      (unless (= current-pos (car-safe vimp-jump-list))
        (add-to-list 'vimp-jump-list current-pos)))))

(vimp-define-motion vimp-jump-forward (count)
  "Go to newer position in jump list.
To go the other way, press \
\\<vimp-motion-state-map>\\[vimp-jump-backward]."
  (let ((count (or count 1))
        current-pos next-pos)
    (vimp-motion-loop (nil count)
      (setq current-pos (car-safe vimp-jump-list)
            next-pos (car (cdr-safe vimp-jump-list)))
      (when next-pos
        (push-mark current-pos t nil)
        (unless (eq (marker-buffer next-pos) (current-buffer))
          (switch-to-buffer (marker-buffer next-pos)))
        (goto-char next-pos)
        (pop vimp-jump-list)))))

(vimp-define-motion vimp-jump-to-tag (arg)
  "Jump to tag under point.
If called with a prefix argument, provide a prompt
for specifying the tag."
  :jump t
  (interactive "P")
  (if arg (call-interactively #'find-tag)
    (let ((tag (funcall (or find-tag-default-function
                            (get major-mode 'find-tag-default-function)
                            #'find-tag-default))))
      (unless tag (user-error "No tag candidate found around point"))
      (find-tag tag))))

(vimp-define-motion vimp-lookup ()
  "Look up the keyword at point.
Calls `vimp-lookup-func'."
  (funcall vimp-lookup-func))

(defun vimp-ret-gen (count indent?)
  (let* ((field  (get-char-property (point) 'field))
         (button (get-char-property (point) 'button))
         (doc    (get-char-property (point) 'widget-doc))
         (widget (or field button doc)))
    (cond
     ((and widget
           (fboundp 'widget-type)
           (fboundp 'widget-button-press)
           (or (and (symbolp widget)
                    (get widget 'widget-type))
               (and (consp widget)
                    (get (widget-type widget) 'widget-type))))
      (when (vimp-operator-state-p)
        (setq vimp-inhibit-operator t))
      (when (fboundp 'widget-button-press)
        (widget-button-press (point))))
     ((and (fboundp 'button-at)
           (fboundp 'push-button)
           (button-at (point)))
      (when (vimp-operator-state-p)
        (setq vimp-inhibit-operator t))
      (push-button))
     ((or (vimp-emacs-state-p)
          (and (vimp-insert-state-p)
               (not buffer-read-only)))
      (if (not indent?)
          (newline count)
        (delete-horizontal-space t)
        (newline count)
        (indent-according-to-mode)))
     (t
      (vimp-next-line-first-non-blank count)))))

(vimp-define-motion vimp-ret (count)
  "Move the cursor COUNT lines down.
If point is on a widget or a button, click on it.
In Insert state, insert a newline."
  :type line
  (vimp-ret-gen count nil))

(vimp-define-motion vimp-ret-and-indent (count)
  "Move the cursor COUNT lines down.
If point is on a widget or a button, click on it.
In Insert state, insert a newline and indent."
  :type line
  (vimp-ret-gen count t))

(vimp-define-motion vimp-window-top (count)
  "Move the cursor to line COUNT from the top of the window
on the first non-blank character."
  :jump t
  :type line
  (move-to-window-line (max (or count 0)
                            (if (= (point-min) (window-start))
                                0
                              scroll-margin)))
  (back-to-indentation))

(vimp-define-motion vimp-window-middle ()
  "Move the cursor to the middle line in the window
on the first non-blank character."
  :jump t
  :type line
  (move-to-window-line
   (/ (1+ (save-excursion (move-to-window-line -1))) 2))
  (back-to-indentation))

(vimp-define-motion vimp-window-bottom (count)
  "Move the cursor to line COUNT from the bottom of the window
on the first non-blank character."
  :jump t
  :type line
  (move-to-window-line (- (max (or count 1) (1+ scroll-margin))))
  (back-to-indentation))

;; scrolling
(vimp-define-command vimp-scroll-line-up (count)
  "Scrolls the window COUNT lines upwards."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (scroll-down count))

(vimp-define-command vimp-scroll-line-down (count)
  "Scrolls the window COUNT lines downwards."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (scroll-up count))

(vimp-define-command vimp-scroll-up (count)
  "Scrolls the window and the cursor COUNT lines upwards.
The default is half the screen."
  :repeat nil
  :keep-visual t
  (interactive "P")
  (vimp-save-column
    (let ((p (point))
          (c (or count (/ (vimp-num-visible-lines) 2))))
      (save-excursion
        (scroll-down (min (vimp-max-scroll-up) c)))
      (forward-line (- c))
      (when (= (line-number-at-pos p)
               (line-number-at-pos (point)))
        (signal 'beginning-of-buffer nil)))))

(vimp-define-command vimp-scroll-down (count)
  "Scrolls the window and the cursor COUNT lines downwards.
The default is half the screen."
  :repeat nil
  :keep-visual t
  (interactive "P")
  (vimp-save-column
    (let ((p (point))
          (c (or count (/ (vimp-num-visible-lines) 2))))
      (save-excursion
        (scroll-up (min (vimp-max-scroll-down) c)))
      (forward-line c)
      (when (= (line-number-at-pos p)
               (line-number-at-pos (point)))
        (signal 'end-of-buffer nil)))))

(vimp-define-command vimp-scroll-page-up (count)
  "Scrolls the window COUNT pages upwards."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (vimp-save-column
    (dotimes (i count)
      (scroll-down nil))))

(vimp-define-command vimp-scroll-page-down (count)
  "Scrolls the window COUNT pages downwards."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (vimp-save-column
    (dotimes (i count)
      (scroll-up nil))))

(vimp-define-command vimp-scroll-line-to-top (count)
  "Scrolls line number COUNT (or the cursor line) to the top of the window."
  :repeat nil
  :keep-visual t
  (interactive "P")
  (vimp-save-column
    (let ((line (or count (line-number-at-pos (point)))))
      (goto-char (point-min))
      (forward-line (1- line)))
    (recenter 0)))

(vimp-define-command vimp-scroll-line-to-center (count)
  "Scrolls line number COUNT (or the cursor line) to the center of the window."
  :repeat nil
  :keep-visual t
  (interactive "P")
  (vimp-save-column
    (when count
      (goto-char (point-min))
      (forward-line (1- count)))
    (recenter nil)))

(vimp-define-command vimp-scroll-line-to-bottom (count)
  "Scrolls line number COUNT (or the cursor line) to the bottom of the window."
  :repeat nil
  :keep-visual t
  (interactive "P")
  (vimp-save-column
    (let ((line (or count (line-number-at-pos (point)))))
      (goto-char (point-min))
      (forward-line (1- line)))
    (recenter -1)))

(vimp-define-command vimp-scroll-bottom-line-to-top (count)
  "Scrolls the line right below the window,
or line COUNT to the top of the window."
  :repeat nil
  :keep-visual t
  (interactive "P")
  (if count
      (progn
        (goto-char (point-min))
        (forward-line (1- count)))
    (goto-char (window-end))
    (vimp-move-cursor-back))
  (recenter 0)
  (vimp-first-non-blank))

(vimp-define-command vimp-scroll-top-line-to-bottom (count)
  "Scrolls the line right below the window,
or line COUNT to the top of the window."
  :repeat nil
  :keep-visual t
  (interactive "P")
  (if count
      (progn
        (goto-char (point-min))
        (forward-line (1- count)))
    (goto-char (window-start)))
  (recenter -1)
  (vimp-first-non-blank))

(vimp-define-command vimp-scroll-left (count)
  "Scrolls the window COUNT half-screenwidths to the left."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (vimp-with-hproject-point-on-window
    (scroll-right (* count (/ (window-width) 2)))))

(vimp-define-command vimp-scroll-right (count)
  "Scrolls the window COUNT half-screenwidths to the right."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (vimp-with-hproject-point-on-window
    (scroll-left (* count (/ (window-width) 2)))))

(vimp-define-command vimp-scroll-column-left (count)
  "Scrolls the window COUNT columns to the left."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (vimp-with-hproject-point-on-window
    (scroll-right count)))

(vimp-define-command vimp-scroll-column-right (count)
  "Scrolls the window COUNT columns to the right."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (vimp-with-hproject-point-on-window
    (scroll-left count)))

;;; Text objects

;; Text objects are defined with `vimp-define-text-object'. In Visual
;; state, they modify the current selection; in Operator-Pending
;; state, they return a pair of buffer positions. Outer text objects
;; are bound in the keymap `vimp-outer-text-objects-map', and inner
;; text objects are bound in `vimp-inner-text-objects-map'.
;;
;; Common text objects like words, WORDS, paragraphs and sentences are
;; defined via a corresponding move-function. This function must have
;; the following properties:
;;
;;   1. Take exactly one argument, the count.
;;   2. When the count is positive, move point forward to the first
;;      character after the end of the next count-th object.
;;   3. When the count is negative, move point backward to the first
;;      character of the count-th previous object.
;;   4. If point is placed on the first character of an object, the
;;      backward motion does NOT count that object.
;;   5. If point is placed on the last character of an object, the
;;      forward motion DOES count that object.
;;   6. The return value is "count left", i.e., in forward direction
;;      count is decreased by one for each successful move and in
;;      backward direction count is increased by one for each
;;      successful move, returning the final value of count.
;;      Therefore, if the complete move is successful, the return
;;      value is 0.
;;
;; A useful macro in this regard is `vimp-motion-loop', which quits
;; when point does not move further and returns the count difference.
;; It also provides a "unit value" of 1 or -1 for use in each
;; iteration. For example, a hypothetical "foo-bar" move could be
;; written as such:
;;
;;     (defun foo-bar (count)
;;       (vimp-motion-loop (var count)
;;         (forward-foo var) ; `var' is 1 or -1 depending on COUNT
;;         (forward-bar var)))
;;
;; If "forward-foo" and "-bar" didn't accept negative arguments,
;; we could choose their backward equivalents by inspecting `var':
;;
;;     (defun foo-bar (count)
;;       (vimp-motion-loop (var count)
;;         (cond
;;          ((< var 0)
;;           (backward-foo 1)
;;           (backward-bar 1))
;;          (t
;;           (forward-foo 1)
;;           (forward-bar 1)))))
;;
;; After a forward motion, point has to be placed on the first
;; character after some object, unless no motion was possible at all.
;; Similarly, after a backward motion, point has to be placed on the
;; first character of some object. This implies that point should
;; NEVER be moved to eob or bob, unless an object ends or begins at
;; eob or bob. (Usually, Emacs motions always move as far as possible.
;; But we want to use the motion-function to identify certain objects
;; in the buffer, and thus exact movement to object boundaries is
;; required.)

(vimp-define-text-object vimp-a-word (count &optional beg end type)
  "Select a word."
  (vimp-select-an-object 'vimp-word beg end type count))

(vimp-define-text-object vimp-inner-word (count &optional beg end type)
  "Select inner word."
  (vimp-select-inner-object 'vimp-word beg end type count))

(vimp-define-text-object vimp-a-WORD (count &optional beg end type)
  "Select a WORD."
  (vimp-select-an-object 'vimp-WORD beg end type count))

(vimp-define-text-object vimp-inner-WORD (count &optional beg end type)
  "Select inner WORD."
  (vimp-select-inner-object 'vimp-WORD beg end type count))

(vimp-define-text-object vimp-a-symbol (count &optional beg end type)
  "Select a symbol."
  (vimp-select-an-object 'vimp-symbol beg end type count))

(vimp-define-text-object vimp-inner-symbol (count &optional beg end type)
  "Select inner symbol."
  (vimp-select-inner-object 'vimp-symbol beg end type count))

(vimp-define-text-object vimp-a-sentence (count &optional beg end type)
  "Select a sentence."
  (vimp-select-an-object 'vimp-sentence beg end type count))

(vimp-define-text-object vimp-inner-sentence (count &optional beg end type)
  "Select inner sentence."
  (vimp-select-inner-object 'vimp-sentence beg end type count))

(vimp-define-text-object vimp-a-paragraph (count &optional beg end type)
  "Select a paragraph."
  :type line
  (vimp-select-an-object 'vimp-paragraph beg end type count t))

(vimp-define-text-object vimp-inner-paragraph (count &optional beg end type)
  "Select inner paragraph."
  :type line
  (vimp-select-inner-object 'vimp-paragraph beg end type count t))

(vimp-define-text-object vimp-a-paren (count &optional beg end type)
  "Select a parenthesis."
  :extend-selection nil
  (vimp-select-paren ?( ?) beg end type count t))

(vimp-define-text-object vimp-inner-paren (count &optional beg end type)
  "Select inner parenthesis."
  :extend-selection nil
  (vimp-select-paren ?( ?) beg end type count))

(vimp-define-text-object vimp-a-bracket (count &optional beg end type)
  "Select a square bracket."
  :extend-selection nil
  (vimp-select-paren ?\[ ?\] beg end type count t))

(vimp-define-text-object vimp-inner-bracket (count &optional beg end type)
  "Select inner square bracket."
  :extend-selection nil
  (vimp-select-paren ?\[ ?\] beg end type count))

(vimp-define-text-object vimp-a-curly (count &optional beg end type)
  "Select a curly bracket (\"brace\")."
  :extend-selection nil
  (vimp-select-paren ?{ ?} beg end type count t))

(vimp-define-text-object vimp-inner-curly (count &optional beg end type)
  "Select inner curly bracket (\"brace\")."
  :extend-selection nil
  (vimp-select-paren ?{ ?} beg end type count))

(vimp-define-text-object vimp-an-angle (count &optional beg end type)
  "Select an angle bracket."
  :extend-selection nil
  (vimp-select-paren ?< ?> beg end type count t))

(vimp-define-text-object vimp-inner-angle (count &optional beg end type)
  "Select inner angle bracket."
  :extend-selection nil
  (vimp-select-paren ?< ?> beg end type count))

(vimp-define-text-object vimp-a-single-quote (count &optional beg end type)
  "Select a single-quoted expression."
  :extend-selection t
  (vimp-select-quote ?' beg end type count t))

(vimp-define-text-object vimp-inner-single-quote (count &optional beg end type)
  "Select inner single-quoted expression."
  :extend-selection nil
  (vimp-select-quote ?' beg end type count))

(vimp-define-text-object vimp-a-double-quote (count &optional beg end type)
  "Select a double-quoted expression."
  :extend-selection t
  (vimp-select-quote ?\" beg end type count t))

(vimp-define-text-object vimp-inner-double-quote (count &optional beg end type)
  "Select inner double-quoted expression."
  :extend-selection nil
  (vimp-select-quote ?\" beg end type count))

(vimp-define-text-object vimp-a-back-quote (count &optional beg end type)
  "Select a back-quoted expression."
  :extend-selection t
  (vimp-select-quote ?\` beg end type count t))

(vimp-define-text-object vimp-inner-back-quote (count &optional beg end type)
  "Select inner back-quoted expression."
  :extend-selection nil
  (vimp-select-quote ?\` beg end type count))

(vimp-define-text-object vimp-a-tag (count &optional beg end type)
  "Select a tag block."
  :extend-selection nil
  (vimp-select-xml-tag beg end type count t))

(vimp-define-text-object vimp-inner-tag (count &optional beg end type)
  "Select inner tag block."
  :extend-selection nil
  (vimp-select-xml-tag beg end type count))

(vimp-define-text-object vimp-next-match (count &optional beg end type)
  "Select next match."
  (unless (and (boundp 'vimp-search-module)
               (eq vimp-search-module 'vimp-search))
    (user-error "next-match text objects only work with Evil search module."))
  (let ((pnt (point)))
    (cond
     ((eq vimp-ex-search-direction 'forward)
      (unless (eobp) (forward-char))
      (vimp-ex-search-previous 1)
      (when (and (<= vimp-ex-search-match-beg pnt)
                 (> vimp-ex-search-match-end pnt)
                 (not (vimp-visual-state-p)))
        (setq count (1- count)))
      (if (> count 0) (vimp-ex-search-next count)))
     (t
      (unless (eobp) (forward-char))
      (vimp-ex-search-next count))))
  ;; active visual state if command is executed in normal state
  (when (vimp-normal-state-p)
    (vimp-visual-select vimp-ex-search-match-beg vimp-ex-search-match-end 'inclusive +1 t))
  (list vimp-ex-search-match-beg vimp-ex-search-match-end))

(vimp-define-text-object vimp-previous-match (count &optional beg end type)
  "Select next match."
  (unless (and (boundp 'vimp-search-module)
               (eq vimp-search-module 'vimp-search))
    (user-error "previous-match text objects only work with Evil search module."))
  (let ((vimp-ex-search-direction
         (if (eq vimp-ex-search-direction 'backward)
             'forward
           'backward)))
    (vimp-next-match count beg end type)))

;;; Operator commands

(vimp-define-operator vimp-yank (beg end type register yank-handler)
  "Saves the characters in motion into the kill-ring."
  :move-point nil
  :repeat nil
  (interactive "<R><x><y>")
  (let ((vimp-was-yanked-without-register
         (and vimp-was-yanked-without-register (not register))))
    (cond
     ((and (fboundp 'cua--global-mark-active)
           (fboundp 'cua-copy-region-to-global-mark)
           (cua--global-mark-active))
      (cua-copy-region-to-global-mark beg end))
     ((eq type 'block)
      (vimp-yank-rectangle beg end register yank-handler))
     ((eq type 'line)
      (vimp-yank-lines beg end register yank-handler))
     (t
      (vimp-yank-characters beg end register yank-handler)))))

(vimp-define-operator vimp-yank-line (beg end type register)
  "Saves whole lines into the kill-ring."
  :motion vimp-line
  :move-point nil
  (interactive "<R><x>")
  (when (vimp-visual-state-p)
    (unless (memq type '(line block))
      (let ((range (vimp-expand beg end 'line)))
        (setq beg (vimp-range-beginning range)
              end (vimp-range-end range)
              type (vimp-type range))))
    (vimp-exit-visual-state))
  (vimp-yank beg end type register))

(vimp-define-operator vimp-delete (beg end type register yank-handler)
  "Delete text from BEG to END with TYPE.
Save in REGISTER or in the kill-ring with YANK-HANDLER."
  (interactive "<R><x><y>")
  (unless register
    (let ((text (filter-buffer-substring beg end)))
      (unless (string-match-p "\n" text)
        ;; set the small delete register
        (vimp-set-register ?- text))))
  (let ((vimp-was-yanked-without-register nil))
    (vimp-yank beg end type register yank-handler))
  (cond
   ((eq type 'block)
    (vimp-apply-on-block #'delete-region beg end nil))
   ((and (eq type 'line)
         (= end (point-max))
         (or (= beg end)
             (/= (char-before end) ?\n))
         (/= beg (point-min))
         (=  (char-before beg) ?\n))
    (delete-region (1- beg) end))
   (t
    (delete-region beg end)))
  ;; place cursor on beginning of line
  (when (and (vimp-called-interactively-p)
             (eq type 'line))
    (vimp-first-non-blank)))

(vimp-define-operator vimp-delete-line (beg end type register yank-handler)
  "Delete to end of line."
  :motion nil
  :keep-visual t
  (interactive "<R><x>")
  ;; act linewise in Visual state
  (let* ((beg (or beg (point)))
         (end (or end beg)))
    (when (vimp-visual-state-p)
      (unless (memq type '(line block))
        (let ((range (vimp-expand beg end 'line)))
          (setq beg (vimp-range-beginning range)
                end (vimp-range-end range)
                type (vimp-type range))))
      (vimp-exit-visual-state))
    (cond
     ((eq type 'block)
      ;; equivalent to $d, i.e., we use the block-to-eol selection and
      ;; call `vimp-delete'. In this case we fake the call to
      ;; `vimp-end-of-line' by setting `temporary-goal-column' and
      ;; `last-command' appropriately as `vimp-end-of-line' would do.
      (let ((temporary-goal-column most-positive-fixnum)
            (last-command 'next-line))
        (vimp-delete beg end 'block register yank-handler)))
     ((eq type 'line)
      (vimp-delete beg end type register yank-handler))
     (t
      (vimp-delete beg (line-end-position) type register yank-handler)))))

(vimp-define-operator vimp-delete-whole-line
  (beg end type register yank-handler)
  "Delete whole line."
  :motion vimp-line
  (interactive "<R><x>")
  (vimp-delete beg end type register yank-handler))

(vimp-define-operator vimp-delete-char (beg end type register)
  "Delete next character."
  :motion vimp-forward-char
  (interactive "<R><x>")
  (vimp-delete beg end type register))

(vimp-define-operator vimp-delete-backward-char (beg end type register)
  "Delete previous character."
  :motion vimp-backward-char
  (interactive "<R><x>")
  (vimp-delete beg end type register))

(vimp-define-command vimp-delete-backward-char-and-join (count)
  "Delete previous character and join lines.
If point is at the beginning of a line then the current line will
be joined with the previous line if and only if
`vimp-backspace-join-lines'."
  (interactive "p")
  (if (or vimp-backspace-join-lines (not (bolp)))
      (call-interactively 'delete-backward-char)
    (user-error "Beginning of line")))

(vimp-define-command vimp-delete-backward-word ()
  "Delete previous word."
  (if (and (bolp) (not (bobp)))
      (progn
        (unless vimp-backspace-join-lines (user-error "Beginning of line"))
        (delete-char -1))
    (vimp-delete (max
                  (save-excursion
                    (vimp-backward-word-begin)
                    (point))
                  (line-beginning-position))
                 (point)
                 'exclusive
                 nil)))

(vimp-define-operator vimp-change
  (beg end type register yank-handler delete-func)
  "Change text from BEG to END with TYPE.
Save in REGISTER or the kill-ring with YANK-HANDLER.
DELETE-FUNC is a function for deleting text, default `vimp-delete'.
If TYPE is `line', insertion starts on an empty line.
If TYPE is `block', the inserted text in inserted at each line
of the block."
  (interactive "<R><x><y>")
  (let ((delete-func (or delete-func #'vimp-delete))
        (nlines (1+ (- (line-number-at-pos end)
                       (line-number-at-pos beg))))
        (opoint (save-excursion
                  (goto-char beg)
                  (line-beginning-position))))
    (funcall delete-func beg end type register yank-handler)
    (cond
     ((eq type 'line)
      (if ( = opoint (point))
          (vimp-open-above 1)
        (vimp-open-below 1)))
     ((eq type 'block)
      (vimp-insert 1 nlines))
     (t
      (vimp-insert 1)))))

(vimp-define-operator vimp-change-line (beg end type register yank-handler)
  "Change to end of line."
  :motion vimp-end-of-line
  (interactive "<R><x><y>")
  (vimp-change beg end type register yank-handler #'vimp-delete-line))

(vimp-define-operator vimp-change-whole-line
  (beg end type register yank-handler)
  "Change whole line."
  :motion vimp-line
  (interactive "<R><x>")
  (vimp-change beg end type register yank-handler #'vimp-delete-whole-line))

(vimp-define-command vimp-copy (beg end address)
  "Copy lines in BEG END below line given by ADDRESS."
  :motion vimp-line
  (interactive "<r><addr>")
  (goto-char (point-min))
  (forward-line address)
  (let* ((txt (buffer-substring-no-properties beg end))
         (len (length txt)))
    ;; ensure text consists of complete lines
    (when (or (zerop len) (/= (aref txt (1- len)) ?\n))
      (setq txt (concat txt "\n")))
    (when (and (eobp) (not (bolp))) (newline)) ; incomplete last line
    (insert txt)
    (forward-line -1)))

(vimp-define-command vimp-move (beg end address)
  "Move lines in BEG END below line given by ADDRESS."
  :motion vimp-line
  (interactive "<r><addr>")
  (goto-char (point-min))
  (forward-line address)
  (let* ((m (set-marker (make-marker) (point)))
         (txt (buffer-substring-no-properties beg end))
         (len (length txt)))
    (delete-region beg end)
    (goto-char m)
    (set-marker m nil)
    ;; ensure text consists of complete lines
    (when (or (zerop len) (/= (aref txt (1- len)) ?\n))
      (setq txt (concat txt "\n")))
    (when (and (eobp) (not (bolp))) (newline)) ; incomplete last line
    (insert txt)
    (forward-line -1)))

(vimp-define-operator vimp-substitute (beg end type register)
  "Change a character."
  :motion vimp-forward-char
  (interactive "<R><x>")
  (vimp-change beg end type register))

(vimp-define-operator vimp-upcase (beg end type)
  "Convert text to upper case."
  (if (eq type 'block)
      (vimp-apply-on-block #'vimp-upcase beg end nil)
    (upcase-region beg end)))

(vimp-define-operator vimp-downcase (beg end type)
  "Convert text to lower case."
  (if (eq type 'block)
      (vimp-apply-on-block #'vimp-downcase beg end nil)
    (downcase-region beg end)))

(vimp-define-operator vimp-invert-case (beg end type)
  "Invert case of text."
  (let (char)
    (if (eq type 'block)
        (vimp-apply-on-block #'vimp-invert-case beg end nil)
      (save-excursion
        (goto-char beg)
        (while (< beg end)
          (setq char (following-char))
          (delete-char 1 nil)
          (if (eq (upcase char) char)
              (insert-char (downcase char) 1)
            (insert-char (upcase char) 1))
          (setq beg (1+ beg)))))))

(vimp-define-operator vimp-invert-char (beg end type)
  "Invert case of character."
  :motion vimp-forward-char
  (if (eq type 'block)
      (vimp-apply-on-block #'vimp-invert-case beg end nil)
    (vimp-invert-case beg end)
    (when vimp-this-motion
      (goto-char end)
      (when (and vimp-cross-lines
                 vimp-move-cursor-back
                 (not vimp-move-beyond-eol)
                 (not (vimp-visual-state-p))
                 (not (vimp-operator-state-p))
                 (eolp) (not (eobp)) (not (bolp)))
        (forward-char)))))

(vimp-define-operator vimp-rot13 (beg end type)
  "ROT13 encrypt text."
  (if (eq type 'block)
      (vimp-apply-on-block #'vimp-rot13 beg end nil)
    (rot13-region beg end)))

(vimp-define-operator vimp-join (beg end)
  "Join the selected lines."
  :motion vimp-line
  (let ((count (count-lines beg end)))
    (when (> count 1)
      (setq count (1- count)))
    (dotimes (var count)
      (join-line 1))))

(vimp-define-operator vimp-join-whitespace (beg end)
  "Join the selected lines without changing whitespace.
\\<vimp-normal-state-map>Like \\[vimp-join], \
but doesn't insert or remove any spaces."
  :motion vimp-line
  (let ((count (count-lines beg end)))
    (when (> count 1)
      (setq count (1- count)))
    (dotimes (var count)
      (vimp-move-end-of-line 1)
      (unless (eobp)
        (delete-char 1)))))

(vimp-define-operator vimp-fill (beg end)
  "Fill text."
  :move-point nil
  :type line
  (save-excursion
    (condition-case nil
        (fill-region beg end)
      (error nil))))

(vimp-define-operator vimp-fill-and-move (beg end)
  "Fill text and move point to the end of the filled region."
  :move-point nil
  :type line
  (let ((marker (make-marker)))
    (move-marker marker (1- end))
    (condition-case nil
        (progn
          (fill-region beg end)
          (goto-char marker)
          (vimp-first-non-blank))
      (error nil))))

(vimp-define-operator vimp-indent (beg end)
  "Indent text."
  :move-point nil
  :type line
  (if (and (= beg (line-beginning-position))
           (= end (line-beginning-position 2)))
      ;; since some Emacs modes can only indent one line at a time,
      ;; implement "==" as a call to `indent-according-to-mode'
      (indent-according-to-mode)
    (goto-char beg)
    (indent-region beg end))
  ;; We also need to tabify or untabify the leading white characters
  (let* ((beg-line (line-number-at-pos beg))
         (end-line (line-number-at-pos end))
         (ln beg-line)
         (convert-white (if indent-tabs-mode 'tabify 'untabify)))
    (save-excursion
      (while (<= ln end-line)
        (goto-char (point-min))
        (forward-line (- ln 1))
        (back-to-indentation)
        ;; Whether tab or space should be used is determined by indent-tabs-mode
        (funcall convert-white (line-beginning-position) (point))
        (setq ln (1+ ln)))))
  (back-to-indentation))

(vimp-define-operator vimp-indent-line (beg end)
  "Indent the line."
  :motion vimp-line
  (vimp-indent beg end))

(vimp-define-operator vimp-shift-left (beg end &optional count preserve-empty)
  "Shift text from BEG to END to the left.
The text is shifted to the nearest multiple of `vimp-shift-width'
\(the rounding can be disabled by setting `vimp-shift-round').
If PRESERVE-EMPTY is non-nil, lines that contain only spaces are
indented, too, otherwise they are ignored.  The relative column
of point is preserved if this function is not called
interactively. Otherwise, if the function is called as an
operator, point is moved to the first non-blank character.
See also `vimp-shift-right'."
  :type line
  (interactive "<r><vc>")
  (vimp-shift-right beg end (- (or count 1)) preserve-empty))

(vimp-define-operator vimp-shift-right (beg end &optional count preserve-empty)
  "Shift text from BEG to END to the right.
The text is shifted to the nearest multiple of `vimp-shift-width'
\(the rounding can be disabled by setting `vimp-shift-round').
If PRESERVE-EMPTY is non-nil, lines that contain only spaces are
indented, too, otherwise they are ignored.  The relative column
of point is preserved if this function is not called
interactively. Otherwise, if the function is called as an
operator, point is moved to the first non-blank character.
See also `vimp-shift-left'."
  :type line
  (interactive "<r><vc>")
  (setq count (or count 1))
  (let ((beg (set-marker (make-marker) beg))
        (end (set-marker (make-marker) end))
        (pnt-indent (current-column))
        first-shift) ; shift of first line
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (let* ((indent (current-indentation))
               (new-indent
                (max 0
                     (if (not vimp-shift-round)
                         (+ indent (* count vimp-shift-width))
                       (* (+ (/ indent vimp-shift-width)
                             count
                             (cond
                              ((> count 0) 0)
                              ((zerop (mod indent vimp-shift-width)) 0)
                              (t 1)))
                          vimp-shift-width)))))
          (unless first-shift
            (setq first-shift (- new-indent indent)))
          (when (or preserve-empty
                    (save-excursion
                      (skip-chars-forward " \t")
                      (not (eolp))))
            (indent-to new-indent 0))
          (delete-region (point) (progn (skip-chars-forward " \t") (point)))
          (forward-line 1))))
    ;; assuming that point is in the first line, adjust its position
    (if (called-interactively-p 'any)
        (vimp-first-non-blank)
      (move-to-column (max 0 (+ pnt-indent first-shift))))))

(vimp-define-command vimp-shift-right-line (count)
  "Shift the current line COUNT times to the right.
The text is shifted to the nearest multiple of
`vimp-shift-width'. Like `vimp-shift-right' but always works on
the current line."
  (interactive "<c>")
  (vimp-shift-right (line-beginning-position) (line-beginning-position 2) count t))

(vimp-define-command vimp-shift-left-line (count)
  "Shift the current line COUNT times to the left.
The text is shifted to the nearest multiple of
`vimp-shift-width'. Like `vimp-shift-left' but always works on
the current line."
  (interactive "<c>")
  (vimp-shift-left (line-beginning-position) (line-beginning-position 2) count t))

(vimp-define-operator vimp-align-left (beg end type &optional width)
  "Right-align lines in the region at WIDTH columns.
The default for width is the value of `fill-column'."
  :motion vimp-line
  :type line
  (interactive "<R><a>")
  (vimp-justify-lines beg end 'left (if width
                                        (string-to-number width)
                                      0)))

(vimp-define-operator vimp-align-right (beg end type &optional width)
  "Right-align lines in the region at WIDTH columns.
The default for width is the value of `fill-column'."
  :motion vimp-line
  :type line
  (interactive "<R><a>")
  (vimp-justify-lines beg end 'right (if width
                                         (string-to-number width)
                                       fill-column)))

(vimp-define-operator vimp-align-center (beg end type &optional width)
  "Centers lines in the region between WIDTH columns.
The default for width is the value of `fill-column'."
  :motion vimp-line
  :type line
  (interactive "<R><a>")
  (vimp-justify-lines beg end 'center (if width
                                          (string-to-number width)
                                        fill-column)))

(vimp-define-operator vimp-replace (beg end type char)
  "Replace text from BEG to END with CHAR."
  :motion vimp-forward-char
  (interactive "<R>"
               (vimp-save-cursor
                 (vimp-refresh-cursor 'replace)
                 (list (vimp-read-key))))
  (when char
    (if (eq type 'block)
        (save-excursion
          (vimp-apply-on-rectangle
           #'(lambda (begcol endcol char)
               (let ((maxcol (vimp-column (line-end-position))))
                 (when (< begcol maxcol)
                   (setq endcol (min endcol maxcol))
                   (let ((beg (vimp-move-to-column begcol nil t))
                         (end (vimp-move-to-column endcol nil t)))
                     (delete-region beg end)
                     (insert (make-string (- endcol begcol) char))))))
           beg end char))
      (goto-char beg)
      (cond
       ((eq char ?\n)
        (delete-region beg end)
        (newline)
        (when vimp-auto-indent
          (indent-according-to-mode)))
       (t
        (while (< (point) end)
          (if (eq (char-after) ?\n)
              (forward-char)
            (delete-char 1)
            (insert-char char 1)))
        (goto-char (max beg (1- end))))))))

(vimp-define-command vimp-paste-before
  (count &optional register yank-handler)
  "Pastes the latest yanked text before the cursor position.
The return value is the yanked text."
  :suppress-operator t
  (interactive "P<x>")
  (if (vimp-visual-state-p)
      (vimp-visual-paste count register)
    (vimp-with-undo
      (let* ((text (if register
                       (vimp-get-register register)
                     (current-kill 0)))
             (yank-handler (or yank-handler
                               (when (stringp text)
                                 (car-safe (get-text-property
                                            0 'yank-handler text)))))
             (opoint (point)))
        (when text
          (if (functionp yank-handler)
              (let ((vimp-paste-count count)
                    ;; for non-interactive use
                    (this-command #'vimp-paste-before))
                (push-mark opoint t)
                (insert-for-yank text))
            ;; no yank-handler, default
            (when (vectorp text)
              (setq text (vimp-vector-to-string text)))
            (set-text-properties 0 (length text) nil text)
            (push-mark opoint t)
            (dotimes (i (or count 1))
              (insert-for-yank text))
            (setq vimp-last-paste
                  (list #'vimp-paste-before
                        count
                        opoint
                        opoint    ; beg
                        (point))) ; end
            (vimp-set-marker ?\[ opoint)
            (vimp-set-marker ?\] (1- (point)))
            (when (> (length text) 0)
              (backward-char))))
        ;; no paste-pop after pasting from a register
        (when register
          (setq vimp-last-paste nil))
        (and (> (length text) 0) text)))))

(vimp-define-command vimp-paste-after
  (count &optional register yank-handler)
  "Pastes the latest yanked text behind point.
The return value is the yanked text."
  :suppress-operator t
  (interactive "P<x>")
  (if (vimp-visual-state-p)
      (vimp-visual-paste count register)
    (vimp-with-undo
      (let* ((text (if register
                       (vimp-get-register register)
                     (current-kill 0)))
             (yank-handler (or yank-handler
                               (when (stringp text)
                                 (car-safe (get-text-property
                                            0 'yank-handler text)))))
             (opoint (point)))
        (when text
          (if (functionp yank-handler)
              (let ((vimp-paste-count count)
                    ;; for non-interactive use
                    (this-command #'vimp-paste-after))
                (insert-for-yank text))
            ;; no yank-handler, default
            (when (vectorp text)
              (setq text (vimp-vector-to-string text)))
            (set-text-properties 0 (length text) nil text)
            (unless (eolp) (forward-char))
            (push-mark (point) t)
            ;; TODO: Perhaps it is better to collect a list of all
            ;; (point . mark) pairs to undo the yanking for COUNT > 1.
            ;; The reason is that this yanking could very well use
            ;; `yank-handler'.
            (let ((beg (point)))
              (dotimes (i (or count 1))
                (insert-for-yank text))
              (setq vimp-last-paste
                    (list #'vimp-paste-after
                          count
                          opoint
                          beg       ; beg
                          (point))) ; end
              (vimp-set-marker ?\[ beg)
              (vimp-set-marker ?\] (1- (point)))
              (when (vimp-normal-state-p)
                (vimp-move-cursor-back)))))
        (when register
          (setq vimp-last-paste nil))
        (and (> (length text) 0) text)))))

(vimp-define-command vimp-visual-paste (count &optional register)
  "Paste over Visual selection."
  :suppress-operator t
  (interactive "P<x>")
  ;; vimp-visual-paste is typically called from vimp-paste-before or
  ;; vimp-paste-after, but we have to mark that the paste was from
  ;; visual state
  (setq this-command 'vimp-visual-paste)
  (let* ((text (if register
                   (vimp-get-register register)
                 (current-kill 0)))
         (yank-handler (car-safe (get-text-property
                                  0 'yank-handler text)))
         new-kill
         paste-eob)
    (vimp-with-undo
      (let* ((kill-ring (list (current-kill 0)))
             (kill-ring-yank-pointer kill-ring))
        (when (vimp-visual-state-p)
          (vimp-visual-rotate 'upper-left)
          ;; if we replace the last buffer line that does not end in a
          ;; newline, we use `vimp-paste-after' because `vimp-delete'
          ;; will move point to the line above
          (when (and (= vimp-visual-end (point-max))
                     (/= (char-before (point-max)) ?\n))
            (setq paste-eob t))
          (vimp-delete vimp-visual-beginning vimp-visual-end
                       (vimp-visual-type))
          (when (and (eq yank-handler #'vimp-yank-line-handler)
                     (not (eq (vimp-visual-type) 'line))
                     (not (= vimp-visual-end (point-max))))
            (insert "\n"))
          (vimp-normal-state)
          (setq new-kill (current-kill 0))
          (current-kill 1))
        (if paste-eob
            (vimp-paste-after count register)
          (vimp-paste-before count register)))
      (kill-new new-kill)
      ;; mark the last paste as visual-paste
      (setq vimp-last-paste
            (list (nth 0 vimp-last-paste)
                  (nth 1 vimp-last-paste)
                  (nth 2 vimp-last-paste)
                  (nth 3 vimp-last-paste)
                  (nth 4 vimp-last-paste)
                  t)))))

(defun vimp-paste-from-register (register)
  "Paste from REGISTER."
  (interactive
   (let ((overlay (make-overlay (point) (point)))
         (string "\""))
     (unwind-protect
         (progn
           ;; display " in the buffer while reading register
           (put-text-property 0 1 'face 'minibuffer-prompt string)
           (put-text-property 0 1 'cursor t string)
           (overlay-put overlay 'after-string string)
           (list (or vimp-this-register (read-char))))
       (delete-overlay overlay))))
  (when (vimp-paste-before nil register t)
    ;; go to end of pasted text
    (forward-char)))

(defun vimp-paste-last-insertion ()
  "Paste last insertion."
  (interactive)
  (vimp-paste-from-register ?.))

(vimp-define-command vimp-use-register (register)
  "Use REGISTER for the next command."
  :keep-visual t
  :repeat ignore
  (interactive "<C>")
  (setq vimp-this-register register))

(defvar vimp-macro-buffer nil
  "The buffer that has been active on macro recording.")

(defun vimp-abort-macro ()
  "Abort macro recording when the buffer is changed.
Macros are aborted when the the current buffer
is changed during macro recording."
  (unless (or (minibufferp) (eq (current-buffer) vimp-macro-buffer))
    (remove-hook 'post-command-hook #'vimp-abort-macro)
    (end-kbd-macro)
    (message "Abort macro recording (changed buffer)")))

(vimp-define-command vimp-record-macro (register)
  "Record a keyboard macro into REGISTER.
If REGISTER is :, /, or ?, the corresponding command line window
will be opened instead."
  :keep-visual t
  :suppress-operator t
  (interactive
   (list (unless (and vimp-this-macro defining-kbd-macro)
           (or vimp-this-register (vimp-read-key)))))
  (cond
   ((eq register ?\C-g)
    (keyboard-quit))
   ((and vimp-this-macro defining-kbd-macro)
    (remove-hook 'post-command-hook #'vimp-abort-macro)
    (setq vimp-macro-buffer nil)
    (condition-case nil
        (end-kbd-macro)
      (error nil))
    (when last-kbd-macro
      (when (member last-kbd-macro '("" []))
        (setq last-kbd-macro nil))
      (vimp-set-register vimp-this-macro last-kbd-macro))
    (setq vimp-this-macro nil))
   ((eq register ?:)
    (vimp-command-window-ex))
   ((eq register ?/)
    (vimp-command-window-search-forward))
   ((eq register ??)
    (vimp-command-window-search-backward))
   ((or (and (>= register ?0) (<= register ?9))
        (and (>= register ?a) (<= register ?z))
        (and (>= register ?A) (<= register ?Z)))
    (when defining-kbd-macro (end-kbd-macro))
    (setq vimp-this-macro register)
    (vimp-set-register vimp-this-macro nil)
    (start-kbd-macro nil)
    (setq vimp-macro-buffer (current-buffer))
    (add-hook 'post-command-hook #'vimp-abort-macro))
   (t (error "Invalid register"))))

(vimp-define-command vimp-execute-macro (count macro)
  "Execute keyboard macro MACRO, COUNT times.
When called with a non-numerical prefix \
\(such as \\[universal-argument]),
COUNT is infinite. MACRO is read from a register
when called interactively."
  :keep-visual t
  :suppress-operator t
  (interactive
   (let (count macro register)
     (setq count (if current-prefix-arg
                     (if (numberp current-prefix-arg)
                         current-prefix-arg
                       0) 1)
           register (or vimp-this-register (read-char)))
     (cond
      ((eq register ?@)
       (unless vimp-last-register
         (user-error "No previously executed keyboard macro."))
       (setq macro (vimp-get-register vimp-last-register t)))
      ((eq register ?:)
       (setq macro (lambda () (vimp-ex-repeat nil))))
      (t
       (setq macro (vimp-get-register register t)
             vimp-last-register register)))
     (list count macro)))
  (cond
   ((functionp macro)
    (vimp-repeat-abort)
    (dotimes (i (or count 1))
      (funcall macro)))
   ((or (and (not (stringp macro))
             (not (vectorp macro)))
        (member macro '("" [])))
    ;; allow references to currently empty registers
    ;; when defining macro
    (unless vimp-this-macro
      (user-error "No previous macro")))
   (t
    (condition-case err
        (vimp-with-single-undo
          (execute-kbd-macro macro count))
      ;; enter Normal state if the macro fails
      (error
       (vimp-normal-state)
       (vimp-normalize-keymaps)
       (signal (car err) (cdr err)))))))

;;; Visual commands

(vimp-define-motion vimp-visual-restore ()
  "Restore previous selection."
  (let* ((point (point))
         (mark (or (mark t) point))
         (dir vimp-visual-direction)
         (type (vimp-visual-type))
         range)
    (unless (vimp-visual-state-p)
      (cond
       ;; No previous selection.
       ((or (null vimp-visual-selection)
            (null vimp-visual-mark)
            (null vimp-visual-point)))
       ;; If the type was one-to-one, it is preferable to infer
       ;; point and mark from the selection's boundaries. The reason
       ;; is that a destructive operation may displace the markers
       ;; inside the selection.
       ((vimp-type-property type :one-to-one)
        (setq range (vimp-contract-range (vimp-visual-range))
              mark (vimp-range-beginning range)
              point (vimp-range-end range))
        (when (< dir 0)
          (vimp-swap mark point)))
       ;; If the type wasn't one-to-one, we have to restore the
       ;; selection on the basis of the previous point and mark.
       (t
        (setq mark vimp-visual-mark
              point vimp-visual-point)))
      (vimp-visual-make-selection mark point type t))))

(vimp-define-motion vimp-visual-exchange-corners ()
  "Rearrange corners in Visual Block mode.

        M---+           +---M
        |   |    <=>    |   |
        +---P           P---+

For example, if mark is in the upper left corner and point
in the lower right, this function puts mark in the upper right
corner and point in the lower left."
  (cond
   ((eq vimp-visual-selection 'block)
    (let* ((point (point))
           (mark (or (mark t) point))
           (point-col (vimp-column point))
           (mark-col (vimp-column mark))
           (mark (save-excursion
                   (goto-char mark)
                   (vimp-move-to-column point-col)
                   (point)))
           (point (save-excursion
                    (goto-char point)
                    (vimp-move-to-column mark-col)
                    (point))))
      (vimp-visual-refresh mark point)))
   (t
    (vimp-exchange-point-and-mark)
    (vimp-visual-refresh))))

(vimp-define-command vimp-visual-rotate (corner &optional beg end type)
  "In Visual Block selection, put point in CORNER.
Corner may be one of `upper-left', `upper-right', `lower-left'
and `lower-right':

        upper-left +---+ upper-right
                   |   |
        lower-left +---+ lower-right

When called interactively, the selection is rotated blockwise."
  :keep-visual t
  (interactive
   (let ((corners '(upper-left upper-right lower-right lower-left)))
     (list (or (cadr (memq (vimp-visual-block-corner) corners))
               'upper-left))))
  (let* ((beg (or beg (point)))
         (end (or end (mark t) beg))
         (type (or type vimp-this-type))
         range)
    (cond
     ((memq type '(rectangle block))
      (setq range (vimp-block-rotate beg end :corner corner)
            beg (pop range)
            end (pop range))
      (unless (eq corner (vimp-visual-block-corner corner beg end))
        (vimp-swap beg end))
      (goto-char beg)
      (when (vimp-visual-state-p)
        (vimp-move-mark end)
        (vimp-visual-refresh nil nil nil :corner corner)))
     ((memq corner '(upper-right lower-right))
      (goto-char (max beg end))
      (when (vimp-visual-state-p)
        (vimp-move-mark (min beg end))))
     (t
      (goto-char (min beg end))
      (when (vimp-visual-state-p)
        (vimp-move-mark (max beg end)))))))

;;; Insertion commands

(defun vimp-insert (count &optional vcount skip-empty-lines)
  "Switch to Insert state just before point.
The insertion will be repeated COUNT times and repeated once for
the next VCOUNT - 1 lines starting at the same column.
If SKIP-EMPTY-LINES is non-nil, the insertion will not be performed
on lines on which the insertion point would be after the end of the
lines.  This is the default behaviour for Visual-state insertion."
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (and (vimp-visual-state-p)
              (memq (vimp-visual-type) '(line block))
              (save-excursion
                (let ((m (mark)))
                  ;; go to upper-left corner temporarily so
                  ;; `count-lines' yields accurate results
                  (vimp-visual-rotate 'upper-left)
                  (prog1 (count-lines vimp-visual-beginning vimp-visual-end)
                    (set-mark m)))))
         (vimp-visual-state-p)))
  (if (and (vimp-called-interactively-p)
           (vimp-visual-state-p))
      (cond
       ((eq (vimp-visual-type) 'line)
        (vimp-visual-rotate 'upper-left)
        (vimp-insert-line count vcount))
       ((eq (vimp-visual-type) 'block)
        (let ((column (min (vimp-column vimp-visual-beginning)
                           (vimp-column vimp-visual-end))))
          (vimp-visual-rotate 'upper-left)
          (move-to-column column t)
          (vimp-insert count vcount skip-empty-lines)))
       (t
        (vimp-visual-rotate 'upper-left)
        (vimp-insert count vcount skip-empty-lines)))
    (setq vimp-insert-count count
          vimp-insert-lines nil
          vimp-insert-vcount (and vcount
                                  (> vcount 1)
                                  (list (line-number-at-pos)
                                        (current-column)
                                        vcount))
          vimp-insert-skip-empty-lines skip-empty-lines)
    (vimp-insert-state 1)))

(defun vimp-append (count &optional vcount skip-empty-lines)
  "Switch to Insert state just after point.
The insertion will be repeated COUNT times and repeated once for
the next VCOUNT - 1 lines starting at the same column.  If
SKIP-EMPTY-LINES is non-nil, the insertion will not be performed
on lines on which the insertion point would be after the end of
the lines."
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (and (vimp-visual-state-p)
              (memq (vimp-visual-type) '(line block))
              (save-excursion
                (let ((m (mark)))
                  ;; go to upper-left corner temporarily so
                  ;; `count-lines' yields accurate results
                  (vimp-visual-rotate 'upper-left)
                  (prog1 (count-lines vimp-visual-beginning vimp-visual-end)
                    (set-mark m)))))))
  (if (and (vimp-called-interactively-p)
           (vimp-visual-state-p))
      (cond
       ((or (eq (vimp-visual-type) 'line)
            (and (eq (vimp-visual-type) 'block)
                 (memq last-command '(next-line previous-line))
                 (numberp temporary-goal-column)
                 (= temporary-goal-column most-positive-fixnum)))
        (vimp-visual-rotate 'upper-left)
        (vimp-append-line count vcount))
       ((eq (vimp-visual-type) 'block)
        (let ((column (max (vimp-column vimp-visual-beginning)
                           (vimp-column vimp-visual-end))))
          (vimp-visual-rotate 'upper-left)
          (move-to-column column t)
          (vimp-insert count vcount skip-empty-lines)))
       (t
        (vimp-visual-rotate 'lower-right)
        (vimp-append count)))
    (unless (eolp) (forward-char))
    (vimp-insert count vcount skip-empty-lines)))

(defun vimp-insert-resume (count)
  "Switch to Insert state at previous insertion point.
The insertion will be repeated COUNT times."
  (interactive "p")
  (vimp-goto-mark ?^ t)
  (vimp-insert count))

(defun vimp-maybe-remove-spaces ()
  "Remove space from newly opened empty line.
This function should be called from `post-command-hook' after
`vimp-open-above' or `vimp-open-below'.  If the last command
finished insert state and if the current line consists of
whitespaces only, then those spaces have been inserted because of
the indentation.  In this case those spaces are removed leaving a
completely empty line."
  (unless (memq this-command '(vimp-open-above vimp-open-below))
    (remove-hook 'post-command-hook 'vimp-maybe-remove-spaces)
    (when (and (not (vimp-insert-state-p))
               (save-excursion
                 (beginning-of-line)
                 (looking-at "^\\s-*$")))
      (delete-region (line-beginning-position)
                     (line-end-position)))))

(defun vimp-open-above (count)
  "Insert a new line above point and switch to Insert state.
The insertion will be repeated COUNT times."
  (interactive "p")
  (vimp-insert-newline-above)
  (setq vimp-insert-count count
        vimp-insert-lines t
        vimp-insert-vcount nil)
  (vimp-insert-state 1)
  (add-hook 'post-command-hook #'vimp-maybe-remove-spaces)
  (when vimp-auto-indent
    (indent-according-to-mode)))

(defun vimp-open-below (count)
  "Insert a new line below point and switch to Insert state.
The insertion will be repeated COUNT times."
  (interactive "p")
  (vimp-insert-newline-below)
  (setq vimp-insert-count count
        vimp-insert-lines t
        vimp-insert-vcount nil)
  (vimp-insert-state 1)
  (add-hook 'post-command-hook #'vimp-maybe-remove-spaces)
  (when vimp-auto-indent
    (indent-according-to-mode)))

(defun vimp-insert-line (count &optional vcount)
  "Switch to insert state at beginning of current line.
Point is placed at the first non-blank character on the current
line.  The insertion will be repeated COUNT times.  If VCOUNT is
non nil it should be number > 0. The insertion will be repeated
in the next VCOUNT - 1 lines below the current one."
  (interactive "p")
  (back-to-indentation)
  (setq vimp-insert-count count
        vimp-insert-lines nil
        vimp-insert-vcount
        (and vcount
             (> vcount 1)
             (list (line-number-at-pos)
                   #'vimp-first-non-blank
                   vcount)))
  (vimp-insert-state 1))

(defun vimp-append-line (count &optional vcount)
  "Switch to Insert state at the end of the current line.
The insertion will be repeated COUNT times.  If VCOUNT is non nil
it should be number > 0. The insertion will be repeated in the
next VCOUNT - 1 lines below the current one."
  (interactive "p")
  (vimp-move-end-of-line)
  (setq vimp-insert-count count
        vimp-insert-lines nil
        vimp-insert-vcount
        (and vcount
             (> vcount 1)
             (list (line-number-at-pos)
                   #'end-of-line
                   vcount)))
  (vimp-insert-state 1))

(vimp-define-command vimp-insert-digraph (count)
  "Insert COUNT digraphs."
  :repeat change
  (interactive "p")
  (let ((digraph (vimp-read-digraph-char 0)))
    (insert-char digraph count)))

(vimp-define-command vimp-ex-show-digraphs ()
  "Shows a list of all available digraphs."
  :repeat nil
  (vimp-with-view-list "vimp-digraphs"
    (let ((i 0)
          (digraphs
           (mapcar #'(lambda (digraph)
                       (cons (cdr digraph)
                             (car digraph)))
                   (append vimp-digraphs-table
                           vimp-digraphs-table-user))))
      (dolist (digraph digraphs)
        (insert (nth 0 digraph) "\t"
                (nth 1 digraph) " "
                (nth 2 digraph)
                (if (= i 2) "\n" "\t\t"))
        (setq i (mod (1+ i) 3))))))

(defun vimp-copy-from-above (arg)
  "Copy characters from preceding non-blank line.
The copied text is inserted before point.
ARG is the number of lines to move backward.
See also \\<vimp-insert-state-map>\\[vimp-copy-from-below]."
  (interactive
   (cond
    ;; if a prefix argument was given, repeat it for subsequent calls
    ((and (null current-prefix-arg)
          (eq last-command #'vimp-copy-from-above))
     (setq current-prefix-arg last-prefix-arg)
     (list (prefix-numeric-value current-prefix-arg)))
    (t
     (list (prefix-numeric-value current-prefix-arg)))))
  (insert (vimp-copy-chars-from-line 1 (- arg))))

(defun vimp-copy-from-below (arg)
  "Copy characters from following non-blank line.
The copied text is inserted before point.
ARG is the number of lines to move forward.
See also \\<vimp-insert-state-map>\\[vimp-copy-from-above]."
  (interactive
   (cond
    ((and (null current-prefix-arg)
          (eq last-command #'vimp-copy-from-below))
     (setq current-prefix-arg last-prefix-arg)
     (list (prefix-numeric-value current-prefix-arg)))
    (t
     (list (prefix-numeric-value current-prefix-arg)))))
  (insert (vimp-copy-chars-from-line 1 arg)))

;; adapted from `copy-from-above-command' in misc.el
(defun vimp-copy-chars-from-line (n num &optional col)
  "Return N characters from line NUM, starting at column COL.
NUM is relative to the current line and can be negative.
COL defaults to the current column."
  (interactive "p")
  (let ((col (or col (current-column))) prefix)
    (save-excursion
      (forward-line num)
      (when (looking-at "[[:space:]]*$")
        (if (< num 0)
            (skip-chars-backward " \t\n")
          (skip-chars-forward " \t\n")))
      (vimp-move-beginning-of-line)
      (move-to-column col)
      ;; if the column winds up in middle of a tab,
      ;; return the appropriate number of spaces
      (when (< col (current-column))
        (if (eq (preceding-char) ?\t)
            (let ((len (min n (- (current-column) col))))
              (setq prefix (make-string len ?\s)
                    n (- n len)))
          ;; if in middle of a control char, return the whole char
          (backward-char 1)))
      (concat prefix
              (buffer-substring (point)
                                (min (line-end-position)
                                     (+ n (point))))))))

;; completion
(vimp-define-command vimp-complete-next (&optional arg)
  "Complete to the nearest following word.
Search backward if a match isn't found.
Calls `vimp-complete-next-func'."
  :repeat change
  (interactive "P")
  (if (minibufferp)
      (funcall vimp-complete-next-minibuffer-func)
    (funcall vimp-complete-next-func arg)))

(vimp-define-command vimp-complete-previous (&optional arg)
  "Complete to the nearest preceding word.
Search forward if a match isn't found.
Calls `vimp-complete-previous-func'."
  :repeat change
  (interactive "P")
  (if (minibufferp)
      (funcall vimp-complete-previous-minibuffer-func)
    (funcall vimp-complete-previous-func arg)))

(vimp-define-command vimp-complete-next-line (&optional arg)
  "Complete a whole line.
Calls `vimp-complete-next-line-func'."
  :repeat change
  (interactive "P")
  (if (minibufferp)
      (funcall vimp-complete-next-minibuffer-func)
    (funcall vimp-complete-next-line-func arg)))

(vimp-define-command vimp-complete-previous-line (&optional arg)
  "Complete a whole line.
Calls `vimp-complete-previous-line-func'."
  :repeat change
  (interactive "P")
  (if (minibufferp)
      (funcall vimp-complete-previous-minibuffer-func)
    (funcall vimp-complete-previous-line-func arg)))

;;; Search

(defun vimp-repeat-search (flag)
  "Called to record a search command.
FLAG is either 'pre or 'post if the function is called before resp.
after executing the command."
  (cond
   ((and (vimp-operator-state-p) (eq flag 'pre))
    (vimp-repeat-record (this-command-keys))
    (vimp-clear-command-keys))
   ((and (vimp-operator-state-p) (eq flag 'post))
    ;; The value of (this-command-keys) at this point should be the
    ;; key-sequence that called the last command that finished the
    ;; search, usually RET. Therefore this key-sequence will be
    ;; recorded in the post-command of the operator. Alternatively we
    ;; could do it here.
    (vimp-repeat-record (if vimp-regexp-search
                            (car-safe regexp-search-ring)
                          (car-safe search-ring))))
   (t (vimp-repeat-motion flag))))

(vimp-define-motion vimp-search-forward ()
  (format "Search forward for user-entered text.
Searches for regular expression if `vimp-regexp-search' is t.%s"
          (if (and (fboundp 'isearch-forward)
                   (documentation 'isearch-forward))
              (format "\n\nBelow is the documentation string \
for `isearch-forward',\nwhich lists available keys:\n\n%s"
                      (documentation 'isearch-forward)) ""))
  :jump t
  :type exclusive
  :repeat vimp-repeat-search
  (vimp-search-incrementally t vimp-regexp-search))

(vimp-define-motion vimp-search-backward ()
  (format "Search backward for user-entered text.
Searches for regular expression if `vimp-regexp-search' is t.%s"
          (if (and (fboundp 'isearch-forward)
                   (documentation 'isearch-forward))
              (format "\n\nBelow is the documentation string \
for `isearch-forward',\nwhich lists available keys:\n\n%s"
                      (documentation 'isearch-forward)) ""))
  :jump t
  :type exclusive
  :repeat vimp-repeat-search
  (vimp-search-incrementally nil vimp-regexp-search))

(vimp-define-motion vimp-search-next (count)
  "Repeat the last search."
  :jump t
  :type exclusive
  (dotimes (var (or count 1))
    (vimp-search (if vimp-regexp-search
                     (car-safe regexp-search-ring)
                   (car-safe search-ring))
                 isearch-forward vimp-regexp-search)))

(vimp-define-motion vimp-search-previous (count)
  "Repeat the last search in the opposite direction."
  :jump t
  :type exclusive
  (dotimes (var (or count 1))
    (vimp-search (if vimp-regexp-search
                     (car-safe regexp-search-ring)
                   (car-safe search-ring))
                 (not isearch-forward) vimp-regexp-search)))

(vimp-define-motion vimp-search-word-backward (count &optional symbol)
  "Search backward for symbol under point."
  :jump t
  :type exclusive
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     vimp-symbol-word-search))
  (dotimes (var (or count 1))
    (vimp-search-word nil nil symbol)))

(vimp-define-motion vimp-search-word-forward (count &optional symbol)
  "Search forward for symbol under point."
  :jump t
  :type exclusive
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     vimp-symbol-word-search))
  (dotimes (var (or count 1))
    (vimp-search-word t nil symbol)))

(vimp-define-motion vimp-search-unbounded-word-backward (count &optiona symbol)
  "Search backward for symbol under point.
The search is unbounded, i.e., the pattern is not wrapped in
\\<...\\>."
  :jump t
  :type exclusive
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     vimp-symbol-word-search))
  (dotimes (var (or count 1))
    (vimp-search-word nil t symbol)))

(vimp-define-motion vimp-search-unbounded-word-forward (count &optiona symbol)
  "Search forward for symbol under point.
The search is unbounded, i.e., the pattern is not wrapped in
\\<...\\>."
  :jump t
  :type exclusive
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     vimp-symbol-word-search))
  (dotimes (var (or count 1))
    (vimp-search-word t t symbol)))

(vimp-define-motion vimp-goto-definition ()
  "Go to definition or first occurrence of symbol under point."
  :jump t
  :type exclusive
  (let* ((string (vimp-find-symbol t))
         (search (format "\\_<%s\\_>" (regexp-quote string)))
         ientry ipos)
    ;; load imenu if available
    (unless (featurep 'imenu)
      (condition-case nil
          (require 'imenu)
        (error nil)))
    (if (null string)
        (user-error "No symbol under cursor")
      (setq isearch-forward t)
      ;; if imenu is available, try it
      (cond
       ((fboundp 'imenu--make-index-alist)
        (condition-case nil
            (setq ientry (imenu--make-index-alist))
          (error nil))
        (setq ientry (assoc string ientry))
        (setq ipos (cdr ientry))
        (when (and (markerp ipos)
                   (eq (marker-buffer ipos) (current-buffer)))
          (setq ipos (marker-position ipos)))
        (cond
         ;; imenu found a position, so go there and
         ;; highlight the occurrence
         ((numberp ipos)
          (vimp-search search t t ipos))
         ;; imenu failed, so just go to first occurrence in buffer
         (t
          (vimp-search search t t (point-min)))))
       ;; no imenu, so just go to first occurrence in buffer
       (t
        (vimp-search search t t (point-min)))))))

;;; Folding
(defun vimp-fold-action (list action)
  "Perform fold ACTION for each matching major or minor mode in LIST.

ACTION will be performed for the first matching handler in LIST.  For more
information on its features and format, see the documentation for
`vimp-fold-list'.

If no matching ACTION is found in LIST, an error will signaled.

Handler errors will be demoted, so a problem in one handler will (hopefully)
not interfere with another."
  (if (null list)
      (user-error
       "Folding is not supported for any of these major/minor modes")
    (let* ((modes (caar list)))
      (if (vimp--mode-p modes)
          (let* ((actions (cdar list))
                 (fn      (plist-get actions action)))
            (when fn
              (with-demoted-errors (funcall fn))))
        (vimp-fold-action (cdr list) action)))))

(defun vimp--mode-p (modes)
  "Determines whether any symbol in MODES represents the current
buffer's major mode or any of its minors."
  (unless (eq modes '())
    (let ((mode (car modes)))
      (or (eq major-mode mode)
          (and (boundp mode) (symbol-value mode))
          (vimp--mode-p (cdr modes))))))

(vimp-define-command vimp-toggle-fold ()
  "Open or close a fold under point.
See also `vimp-open-fold' and `vimp-close-fold'."
  (vimp-fold-action vimp-fold-list :toggle))

(vimp-define-command vimp-open-folds ()
  "Open all folds.
See also `vimp-close-folds'."
  (vimp-fold-action vimp-fold-list :open-all))

(vimp-define-command vimp-close-folds ()
  "Close all folds.
See also `vimp-open-folds'."
  (vimp-fold-action vimp-fold-list :close-all))

(vimp-define-command vimp-open-fold ()
  "Open fold at point.
See also `vimp-close-fold'."
  (vimp-fold-action vimp-fold-list :open))

(vimp-define-command vimp-open-fold-rec ()
  "Open fold at point recursively.
See also `vimp-open-fold' and `vimp-close-fold'."
  (vimp-fold-action vimp-fold-list :open-rec))

(vimp-define-command vimp-close-fold ()
  "Close fold at point.
See also `vimp-open-fold'."
  (vimp-fold-action vimp-fold-list :close))

;;; Ex

(vimp-define-operator vimp-write (beg end type file-or-append &optional bang)
  "Save the current buffer, from BEG to END, to FILE-OR-APPEND.
If FILE-OR-APPEND is of the form \">> FILE\", append to FILE
instead of overwriting.  The current buffer's filename is not
changed unless it has no associated file and no region is
specified.  If the file already exists and the BANG argument is
non-nil, it is overwritten without confirmation."
  :motion nil
  :move-point nil
  :type line
  :repeat nil
  (interactive "<R><fsh><!>")
  (let* ((append-and-filename (vimp-extract-append file-or-append))
         (append (car append-and-filename))
         (filename (cdr append-and-filename))
         (bufname (buffer-file-name (buffer-base-buffer))))
    (when (zerop (length filename))
      (setq filename bufname))
    (cond
     ((zerop (length filename))
      (user-error "Please specify a file name for the buffer"))
     ;; execute command on region
     ((eq (aref filename 0) ?!)
      (shell-command-on-region beg end (substring filename 1)))
     ;; with region or append, always save to file without resetting
     ;; modified flag
     ((or append (and beg end))
      (write-region beg end filename append nil nil (not (or append bang))))
     ;; no current file
     ((null bufname)
      (write-file filename (not bang)))
     ;; save current buffer to its file
     ((string= filename bufname)
      (if (not bang) (save-buffer) (write-file filename)))
     ;; save to another file
     (t
      (write-region nil nil filename
                    nil (not bufname) nil
                    (not bang))))))

(vimp-define-command vimp-write-all (bang)
  "Saves all buffers visiting a file.
If BANG is non nil then read-only buffers are saved, too,
otherwise they are skipped. "
  :repeat nil
  :move-point nil
  (interactive "<!>")
  (if bang
      (save-some-buffers t)
    ;; save only buffer that are not read-only and
    ;; that are visiting a file
    (save-some-buffers t
                       #'(lambda ()
                           (and (not buffer-read-only)
                                (buffer-file-name))))))

(vimp-define-command vimp-save (filename &optional bang)
  "Save the current buffer to FILENAME.
Changes the file name of the current buffer to FILENAME.  If no
FILENAME is given, the current file name is used."
  :repeat nil
  :move-point nil
  (interactive "<f><!>")
  (when (zerop (length filename))
    (setq filename (buffer-file-name (buffer-base-buffer))))
  (write-file filename (not bang)))

(vimp-define-command vimp-edit (file &optional bang)
  "Open FILE.
If no FILE is specified, reload the current buffer from disk."
  :repeat nil
  (interactive "<f><!>")
  (if file
      (find-file file)
    (revert-buffer bang (or bang (not (buffer-modified-p))) t)))

(vimp-define-command vimp-read (count file)
  "Inserts the contents of FILE below the current line or line COUNT."
  :repeat nil
  :move-point nil
  (interactive "P<fsh>")
  (when (and file (not (zerop (length file))))
    (when count (goto-char (point-min)))
    (when (or (not (zerop (forward-line (or count 1))))
              (not (bolp)))
      (insert "\n"))
    (if (/= (aref file 0) ?!)
        (let ((result (insert-file-contents file)))
          (save-excursion
            (forward-char (cadr result))
            (unless (bolp) (insert "\n"))))
      (shell-command (substring file 1) t)
      (save-excursion
        (goto-char (mark))
        (unless (bolp) (insert "\n"))))))

(vimp-define-command vimp-show-files ()
  "Shows the file-list.
The same as `buffer-menu', but shows only buffers visiting
files."
  :repeat nil
  (buffer-menu 1))

(vimp-define-command vimp-goto-error (count)
  "Go to error number COUNT.

If no COUNT supplied, move to the current error.

Acts like `first-error' other than when given no counts, goes
to the current error instead of the first, like in Vim's :cc
command."
  :repeat nil
  (interactive "P")
  (if count
      (first-error (if (eql 0 count) 1 count))
    (next-error 0)))

(vimp-define-command vimp-buffer (buffer)
  "Switches to another buffer."
  :repeat nil
  (interactive "<b>")
  (cond
   ;; no buffer given, switch to "other" buffer
   ((null buffer) (switch-to-buffer (other-buffer)))
   ;; we are given the name of an existing buffer
   ((get-buffer buffer) (switch-to-buffer buffer))
   ;; try to complete the buffer
   ((let ((all-buffers (internal-complete-buffer buffer nil t)))
      (when (= (length all-buffers) 1)
        (switch-to-buffer (car all-buffers)))))
   (t
    (when (y-or-n-p
           (format "No buffer with name \"%s\" exists. Create new buffer? "
                   buffer))
      (switch-to-buffer buffer)))))

(vimp-define-command vimp-next-buffer (&optional count)
  "Goes to the `count'-th next buffer in the buffer list."
  :repeat nil
  (interactive "p")
  (dotimes (i (or count 1))
    (next-buffer)))

(vimp-define-command vimp-prev-buffer (&optional count)
  "Goes to the `count'-th prev buffer in the buffer list."
  :repeat nil
  (interactive "p")
  (dotimes (i (or count 1))
    (previous-buffer)))

(vimp-define-command vimp-delete-buffer (buffer &optional bang)
  "Deletes a buffer.
All windows currently showing this buffer will be closed except
for the last window in each frame."
  (interactive "<b><!>")
  (with-current-buffer (or buffer (current-buffer))
    (when bang
      (set-buffer-modified-p nil)
      (dolist (process (process-list))
        (when (eq (process-buffer process) (current-buffer))
          (set-process-query-on-exit-flag process nil))))
    ;; get all windows that show this buffer
    (let ((wins (get-buffer-window-list (current-buffer) nil t)))
      ;; if the buffer which was initiated by emacsclient,
      ;; call `server-edit' from server.el to avoid
      ;; "Buffer still has clients" message
      (if (and (fboundp 'server-edit)
               (boundp 'server-buffer-clients)
               server-buffer-clients)
          (server-edit)
        (kill-buffer nil))
      ;; close all windows that showed this buffer
      (mapc #'(lambda (w)
                (condition-case nil
                    (delete-window w)
                  (error nil)))
            wins))))

(vimp-define-command vimp-quit (&optional force)
  "Closes the current window, current frame, Emacs.
If the current frame belongs to some client the client connection
is closed."
  :repeat nil
  (interactive "<!>")
  (condition-case nil
      (delete-window)
    (error
     (if (and (boundp 'server-buffer-clients)
              (fboundp 'server-edit)
              (fboundp 'server-buffer-done)
              server-buffer-clients)
         (if force
             (server-buffer-done (current-buffer))
           (server-edit))
       (condition-case nil
           (delete-frame)
         (error
          (if force
              (kill-emacs)
            (save-buffers-kill-emacs))))))))

(vimp-define-command vimp-quit-all (&optional bang)
  "Exits Emacs, asking for saving."
  :repeat nil
  (interactive "<!>")
  (if (null bang)
      (save-buffers-kill-terminal)
    (let ((proc (frame-parameter (selected-frame) 'client)))
      (if proc
          (with-no-warnings
            (server-delete-client proc))
        (dolist (process (process-list))
          (set-process-query-on-exit-flag process nil))
        (kill-emacs)))))

(vimp-define-command vimp-save-and-quit ()
  "Exits Emacs, without saving."
  (save-buffers-kill-terminal t))

(vimp-define-command vimp-save-and-close (file &optional bang)
  "Saves the current buffer and closes the window."
  :repeat nil
  (interactive "<f><!>")
  (vimp-write nil nil nil file bang)
  (vimp-quit))

(vimp-define-command vimp-save-modified-and-close (file &optional bang)
  "Saves the current buffer and closes the window."
  :repeat nil
  (interactive "<f><!>")
  (when (buffer-modified-p)
    (vimp-write nil nil nil file bang))
  (vimp-quit))

(vimp-define-operator vimp-shell-command
  (beg end type command &optional previous)
  "Execute a shell command.
If BEG, END and TYPE is specified, COMMAND is executed on the region,
which is replaced with the command's output. Otherwise, the
output is displayed in its own buffer. If PREVIOUS is non-nil,
the previous shell command is executed instead."
  (interactive "<R><sh><!>")
  (if (not (vimp-ex-p))
      (let ((vimp-ex-initial-input
             (if (and beg
                      (not (vimp-visual-state-p))
                      (not current-prefix-arg))
                 (let ((range (vimp-range beg end type)))
                   (vimp-contract-range range)
                   ;; TODO: this is not exactly the same as Vim, which
                   ;; uses .,+count as range. However, this is easier
                   ;; to achieve with the current implementation and
                   ;; the very inconvenient range interface.
                   ;;
                   ;; TODO: the range interface really needs some
                   ;; rework!
                   (format
                    "%d,%d!"
                    (line-number-at-pos (vimp-range-beginning range))
                    (line-number-at-pos (vimp-range-end range))))
               "!")))
        (call-interactively 'vimp-ex))
    (when command
      (setq command (vimp-ex-replace-special-filenames command)))
    (if (zerop (length command))
        (when previous (setq command vimp-previous-shell-command))
      (setq vimp-previous-shell-command command))
    (cond
     ((zerop (length command))
      (if previous (user-error "No previous shell command")
        (user-error "No shell command")))
     (vimp-ex-range
      (if (not vimp-display-shell-error-in-message)
          (shell-command-on-region beg end command nil t)
        (let ((output-buffer (generate-new-buffer " *temp*"))
              (error-buffer (generate-new-buffer " *temp*")))
          (unwind-protect
              (if (zerop (shell-command-on-region beg end
                                                  command
                                                  output-buffer nil
                                                  error-buffer))
                  (progn
                    (delete-region beg end)
                    (insert-buffer-substring output-buffer)
                    (goto-char beg)
                    (vimp-first-non-blank))
                (display-message-or-buffer error-buffer))
            (kill-buffer output-buffer)
            (kill-buffer error-buffer)))))
     (t
      (shell-command command)))))

(vimp-define-command vimp-make (arg)
  "Call a build command in the current directory.
If ARG is nil this function calls `recompile', otherwise it calls
`compile' passing ARG as build command."
  (interactive "<sh>")
  (if (and (fboundp 'recompile)
           (not arg))
      (recompile)
    (compile arg)))

;; TODO: escape special characters (currently only \n) ... perhaps
;; there is some Emacs function doing this?
(vimp-define-command vimp-show-registers ()
  "Shows the contents of all registers."
  :repeat nil
  (vimp-with-view-list "vimp-registers"
    (setq truncate-lines t)
    (dolist (reg (vimp-register-list))
      (when (cdr reg)
        (insert (format "\"%c\t%s"
                        (car reg)
                        (if (stringp (cdr reg))
                            (replace-regexp-in-string "\n" "^J" (cdr reg))
                          (cdr reg))))
        (newline)))))

(vimp-define-command vimp-show-marks (mrks)
  "Shows all marks.
If MRKS is non-nil it should be a string and only registers
corresponding to the characters of this string are shown."
  :repeat nil
  (interactive "<a>")
  ;; To get markers and positions, we can't rely on 'global-mark-ring'
  ;; provided by Emacs (although it will be much simpler and faster),
  ;; because 'global-mark-ring' does not store mark characters, but
  ;; only buffer name and position. Instead, 'vimp-markers-alist' is
  ;; used; this is list maintained by Evil for each buffer.
  (let ((all-markers
         ;; get global and local marks
         (append (vimp-filter-list #'(lambda (m)
                                       (or (vimp-global-marker-p (car m))
                                           (not (markerp (cdr m)))))
                                   vimp-markers-alist)
                 (vimp-filter-list #'(lambda (m)
                                       (or (not (vimp-global-marker-p
                                                 (car m)))
                                           (not (markerp (cdr m)))))
                                   (default-value 'vimp-markers-alist)))))
    (when mrks
      (setq mrks (string-to-list mrks))
      (setq all-markers (vimp-filter-list #'(lambda (m)
                                              (not (member (car m) mrks)))
                                          all-markers)))
    ;; map marks to list of 4-tuples (char row col file)
    (setq all-markers
          (mapcar #'(lambda (m)
                      (with-current-buffer (marker-buffer (cdr m))
                        (save-excursion
                          (goto-char (cdr m))
                          (list (car m)
                                (1+ (count-lines 1 (line-beginning-position)))
                                (current-column)
                                (buffer-name)))))
                  all-markers))
    (vimp-with-view-list "vimp-marks"
      (setq truncate-lines t)
      (dolist (m (sort all-markers #'(lambda (a b) (< (car a) (car b)))))
        (insert (apply 'format " %c %6d %6d %s\n" m))))))

(eval-when-compile (require 'ffap))
(vimp-define-command vimp-find-file-at-point-with-line ()
  "Opens the file at point and goes to line-number."
  (require 'ffap)
  (let ((fname (with-no-warnings (ffap-file-at-point))))
    (if fname
        (let ((line
               (save-excursion
                 (goto-char (cadr ffap-string-at-point-region))
                 (and (re-search-backward ":\\([0-9]+\\)\\="
                                          (line-beginning-position) t)
                      (string-to-number (match-string 1))))))
          (with-no-warnings (ffap-other-window))
          (when line
            (goto-char (point-min))
            (forward-line (1- line))))
      (user-error "File does not exist."))))

(vimp-ex-define-argument-type state
  "Defines an argument type which can take state names."
  :collection
  (lambda (arg predicate flag)
    (let ((completions
           (append '("nil")
                   (mapcar #'(lambda (state)
                               (format "%s" (car state)))
                           vimp-state-properties))))
      (when arg
        (cond
         ((eq flag nil)
          (try-completion arg completions predicate))
         ((eq flag t)
          (all-completions arg completions predicate))
         ((eq flag 'lambda)
          (test-completion arg completions predicate))
         ((eq (car-safe flag) 'boundaries)
          (cons 'boundaries
                (completion-boundaries arg
                                       completions
                                       predicate
                                       (cdr flag)))))))))

(vimp-define-interactive-code "<state>"
  "A valid vimp state."
  :ex-arg state
  (list (when (and (vimp-ex-p) vimp-ex-argument)
          (intern vimp-ex-argument))))

;; TODO: should we merge this command with `vimp-set-initial-state'?
(vimp-define-command vimp-ex-set-initial-state (state)
  "Set the initial state for the current major mode to STATE.
This is the state the buffer comes up in. See `vimp-set-initial-state'."
  :repeat nil
  (interactive "<state>")
  (if (not (or (assq state vimp-state-properties)
               (null state)))
      (user-error "State %s cannot be set as initial Evil state" state)
    (let ((current-initial-state (vimp-initial-state major-mode)))
      (unless (eq current-initial-state state)
        ;; only if we selected a new mode
        (when (y-or-n-p (format "Major-mode `%s' has initial mode `%s'. \
Change to `%s'? "
                                major-mode
                                (or current-initial-state "DEFAULT")
                                (or state "DEFAULT")))
          (vimp-set-initial-state major-mode state)
          (when (y-or-n-p "Save setting in customization file? ")
            (dolist (s (list current-initial-state state))
              (when s
                (let ((var (intern (format "vimp-%s-state-modes" s))))
                  (customize-save-variable var (symbol-value var)))))))))))

(vimp-define-command vimp-force-normal-state ()
  "Switch to normal state without recording current command."
  :repeat abort
  :suppress-operator t
  (vimp-normal-state))

(vimp-define-motion vimp-ex-search-next (count)
  "Goes to the next occurrence."
  :jump t
  :type exclusive
  (vimp-ex-search count))

(vimp-define-motion vimp-ex-search-previous (count)
  "Goes the the previous occurrence."
  :jump t
  :type exclusive
  (let ((vimp-ex-search-direction
         (if (eq vimp-ex-search-direction 'backward) 'forward 'backward)))
    (vimp-ex-search count)))

(defun vimp-repeat-ex-search (flag)
  "Called to record a search command.
FLAG is either 'pre or 'post if the function is called before
resp.  after executing the command."
  (cond
   ((and (vimp-operator-state-p) (eq flag 'pre))
    (vimp-repeat-record (this-command-keys))
    (vimp-clear-command-keys))
   ((and (vimp-operator-state-p) (eq flag 'post))
    ;; The value of (this-command-keys) at this point should be the
    ;; key-sequence that called the last command that finished the
    ;; search, usually RET. Therefore this key-sequence will be
    ;; recorded in the post-command of the operator. Alternatively we
    ;; could do it here.
    (vimp-repeat-record (vimp-ex-pattern-regex vimp-ex-search-pattern)))
   (t (vimp-repeat-motion flag))))

(vimp-define-motion vimp-ex-search-forward (count)
  "Starts a forward search."
  :jump t
  :type exclusive
  :repeat vimp-repeat-ex-search
  (vimp-ex-start-search 'forward count))

(vimp-define-motion vimp-ex-search-backward (count)
  "Starts a forward search."
  :jump t
  :repeat vimp-repeat-ex-search
  (vimp-ex-start-search 'backward count))

(vimp-define-motion vimp-ex-search-word-forward (count &optional symbol)
  "Search for the next occurrence of word under the cursor."
  :jump t
  :type exclusive
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     vimp-symbol-word-search))
  (vimp-ex-start-word-search nil 'forward count symbol))

(vimp-define-motion vimp-ex-search-word-backward (count &optional symbol)
  "Search for the next occurrence of word under the cursor."
  :jump t
  :type exclusive
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     vimp-symbol-word-search))
  (vimp-ex-start-word-search nil 'backward count symbol))

(vimp-define-motion vimp-ex-search-unbounded-word-forward (count &optional symbol)
  "Search for the next occurrence of word under the cursor."
  :jump t
  :type exclusive
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     vimp-symbol-word-search))
  (vimp-ex-start-word-search t 'forward count symbol))

(vimp-define-motion vimp-ex-search-unbounded-word-backward (count &optional symbol)
  "Search for the next occurrence of word under the cursor."
  :jump t
  :type exclusive
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     vimp-symbol-word-search))
  (vimp-ex-start-word-search t 'backward count symbol))

(vimp-define-operator vimp-ex-substitute
  (beg end pattern replacement flags)
  "The Ex substitute command.
\[BEG,END]substitute/PATTERN/REPLACEMENT/FLAGS"
  :repeat nil
  :jump t
  :move-point nil
  :motion vimp-line
  (interactive "<r><s/>")
  (vimp-ex-nohighlight)
  (unless pattern
    (user-error "No pattern given"))
  (setq replacement (or replacement ""))
  (setq vimp-ex-last-was-search nil)
  (let* ((flags (append flags nil))
         (confirm (memq ?c flags))
         (case-fold-search (vimp-ex-pattern-ignore-case pattern))
         (case-replace case-fold-search)
         (vimp-ex-substitute-regex (vimp-ex-pattern-regex pattern)))
    (setq vimp-ex-substitute-pattern pattern
          vimp-ex-substitute-replacement replacement
          vimp-ex-substitute-flags flags
          isearch-string vimp-ex-substitute-regex)
    (isearch-update-ring vimp-ex-substitute-regex t)
    (if (vimp-ex-pattern-whole-line pattern)
        ;; this one is easy, just use the built-in function
        (perform-replace vimp-ex-substitute-regex
                         vimp-ex-substitute-replacement
                         confirm t nil nil nil
                         beg
                         (if (and (> end (point-min))
                                  (= (char-after (1- end)) ?\n))
                             (1- end)
                           end))
      (let ((vimp-ex-substitute-nreplaced 0)
            (vimp-ex-substitute-last-point (point))
            markers
            transient-mark-mode)
        (save-excursion
          (goto-char beg)
          (beginning-of-line)
          (while (< (point) end)
            (push (move-marker (make-marker) (point)) markers)
            (forward-line)))
        (setq markers (nreverse markers))
        (if confirm
            (let ((vimp-ex-substitute-overlay
                   (make-overlay (point) (point)))
                  (vimp-ex-substitute-hl
                   (vimp-ex-make-hl 'vimp-ex-substitute)))
              (vimp-ex-hl-change 'vimp-ex-substitute pattern)
              (unwind-protect
                  ;; this one is more difficult: we have to do
                  ;; the highlighting and querying on our own
                  (progn
                    (overlay-put vimp-ex-substitute-overlay
                                 'face 'isearch)
                    (overlay-put vimp-ex-substitute-overlay
                                 'priority 1001)
                    (map-y-or-n-p
                     #'(lambda (x)
                         (set-match-data x)
                         (move-overlay vimp-ex-substitute-overlay
                                       (match-beginning 0)
                                       (match-end 0))
                         (format "Query replacing %s with %s: "
                                 (match-string 0)
                                 (vimp-match-substitute-replacement
                                  vimp-ex-substitute-replacement
                                  (not case-replace))))
                     #'(lambda (x)
                         (set-match-data x)
                         (vimp-replace-match vimp-ex-substitute-replacement
                                             (not case-replace))
                         (setq vimp-ex-substitute-last-point (point))
                         (setq vimp-ex-substitute-nreplaced
                               (1+ vimp-ex-substitute-nreplaced))
                         (vimp-ex-hl-set-region 'vimp-ex-substitute
                                                (save-excursion
                                                  (forward-line)
                                                  (point))
                                                (vimp-ex-hl-get-max
                                                 'vimp-ex-substitute)))
                     #'(lambda ()
                         (catch 'found
                           (while markers
                             (let ((m (pop markers)))
                               (goto-char m)
                               (move-marker m nil))
                             (when (re-search-forward vimp-ex-substitute-regex
                                                      (line-end-position) t nil)
                               (goto-char (match-beginning 0))
                               (throw 'found (match-data))))))))
                (vimp-ex-delete-hl 'vimp-ex-substitute)
                (delete-overlay vimp-ex-substitute-overlay)))

          ;; just replace the first occurrences per line
          ;; without highlighting and asking
          (while markers
            (let ((m (pop markers)))
              (goto-char m)
              (move-marker m nil))
            (when (re-search-forward vimp-ex-substitute-regex
                                     (line-end-position) t nil)
              (setq vimp-ex-substitute-nreplaced
                    (1+ vimp-ex-substitute-nreplaced))
              (vimp-replace-match vimp-ex-substitute-replacement
                                  (not case-replace))
              (setq vimp-ex-substitute-last-point (point)))))

        (while markers (move-marker (pop markers) nil))
        (goto-char vimp-ex-substitute-last-point)

        (message "Replaced %d occurrence%s"
                 vimp-ex-substitute-nreplaced
                 (if (/= vimp-ex-substitute-nreplaced 1) "s" ""))))
    (vimp-first-non-blank)))

(vimp-define-operator vimp-ex-repeat-substitute
  (beg end flags)
  "Repeat last substitute command.
This is the same as :s//~/"
  :repeat nil
  :jump t
  :move-point nil
  :motion vimp-line
  (interactive "<r><a>")
  (apply #'vimp-ex-substitute beg end
         (vimp-ex-get-substitute-info (concat "//~/" flags))))

(vimp-define-operator vimp-ex-repeat-substitute-with-flags
  (beg end flags)
  "Repeat last substitute command with last flags.
This is the same as :s//~/&"
  :repeat nil
  :jump t
  :move-point nil
  :motion vimp-line
  (interactive "<r><a>")
  (apply #'vimp-ex-substitute beg end
         (vimp-ex-get-substitute-info (concat "//~/&" flags))))

(vimp-define-operator vimp-ex-repeat-substitute-with-search
  (beg end flags)
  "Repeat last substitute command with last search pattern.
This is the same as :s//~/r"
  :repeat nil
  :jump t
  :move-point nil
  :motion vimp-line
  (interactive "<r><a>")
  (apply #'vimp-ex-substitute beg end
         (vimp-ex-get-substitute-info (concat "//~/r" flags))))

(vimp-define-operator vimp-ex-repeat-substitute-with-search-and-flags
  (beg end flags)
  "Repeat last substitute command with last search pattern and last flags.
This is the same as :s//~/&r"
  :repeat nil
  :jump t
  :move-point nil
  :motion vimp-line
  (interactive "<r><a>")
  (apply #'vimp-ex-substitute beg end
         (vimp-ex-get-substitute-info (concat "//~/&r" flags))))

(vimp-define-operator vimp-ex-repeat-global-substitute ()
  "Repeat last substitute command on the whole buffer.
This is the same as :%s//~/&"
  :repeat nil
  :jump t
  :move-point nil
  :motion vimp-line
  (interactive)
  (apply #'vimp-ex-substitute (point-min) (point-max)
         (vimp-ex-get-substitute-info (concat "//~/&"))))

(vimp-define-operator vimp-ex-global
  (beg end pattern command &optional invert)
  "The Ex global command.
\[BEG,END]global[!]/PATTERN/COMMAND"
  :motion mark-whole-buffer
  :move-point nil
  (interactive "<r><g/><!>")
  (unless pattern
    (user-error "No pattern given"))
  (unless command
    (user-error "No command given"))
  (vimp-with-single-undo
    (let ((case-fold-search
           (eq (vimp-ex-regex-case pattern 'smart) 'insensitive))
          match markers)
      (when (and pattern command)
        (setq isearch-string pattern)
        (isearch-update-ring pattern t)
        (goto-char beg)
        (vimp-move-beginning-of-line)
        (while (< (point) end)
          (setq match (re-search-forward pattern (line-end-position) t))
          (when (or (and match (not invert))
                    (and invert (not match)))
            (push (move-marker (make-marker)
                               (or (and match (match-beginning 0))
                                   (line-beginning-position)))
                  markers))
          (forward-line))
        (setq markers (nreverse markers))
        (unwind-protect
            (dolist (marker markers)
              (goto-char marker)
              (vimp-ex-eval command))
          ;; ensure that all markers are deleted afterwards,
          ;; even in the event of failure
          (dolist (marker markers)
            (set-marker marker nil)))))))

(vimp-define-operator vimp-ex-global-inverted
  (beg end pattern command &optional invert)
  "The Ex vglobal command.
\[BEG,END]vglobal/PATTERN/COMMAND"
  :motion mark-whole-buffer
  :move-point nil
  (interactive "<r><g/><!>")
  (vimp-ex-global beg end pattern command (not invert)))

(vimp-define-operator vimp-ex-normal (beg end commands)
  "The Ex normal command.
Execute the argument as normal command on each line in the
range. The given argument is passed straight to
`execute-kbd-macro'.  The default is the current line."
  :motion vimp-line
  (interactive "<r><a>")
  (vimp-with-single-undo
    (let (markers vimp-ex-current-buffer prefix-arg current-prefix-arg)
      (goto-char beg)
      (while
          (and (< (point) end)
               (progn
                 (push (move-marker (make-marker) (line-beginning-position))
                       markers)
                 (and (= (forward-line) 0) (bolp)))))
      (setq markers (nreverse markers))
      (deactivate-mark)
      (vimp-force-normal-state)
      ;; replace ^[ by escape
      (setq commands
            (vconcat
             (mapcar #'(lambda (ch) (if (equal ch ?) 'escape ch))
                     (append commands nil))))
      (dolist (marker markers)
        (goto-char marker)
        (condition-case nil
            (execute-kbd-macro commands)
          (error nil))
        (vimp-force-normal-state)
        (set-marker marker nil)))))

(vimp-define-command vimp-goto-char (position)
  "Go to POSITION in the buffer.
Default position is the beginning of the buffer."
  (interactive "p")
  (let ((position (vimp-normalize-position
                   (or position (point-min)))))
    (goto-char position)))

(vimp-define-operator vimp-ex-line-number (beg end)
  "Print the last line number."
  :motion mark-whole-buffer
  :move-point nil
  (interactive "<r>")
  (message "%d" (count-lines (point-min) end)))

(vimp-define-command vimp-show-file-info ()
  "Shows basic file information."
  (let* ((nlines   (count-lines (point-min) (point-max)))
         (curr     (line-number-at-pos (point)))
         (perc     (if (> nlines 0)
                       (format "%d%%" (* (/ (float curr) (float nlines)) 100.0))
                     "No lines in buffer"))
         (file     (buffer-file-name (buffer-base-buffer)))
         (writable (and file (file-writable-p file)))
         (readonly (if (and file (not writable)) "[readonly] " "")))
    (if file
        (message "\"%s\" %d %slines --%s--" file nlines readonly perc)
      (message "%d lines --%s--" nlines perc))))

(vimp-define-operator vimp-ex-sort (beg end &optional options reverse)
  "The Ex sort command.
\[BEG,END]sort[!] [i][u]
The following additional options are supported:

  * i   ignore case
  * u   remove duplicate lines

The 'bang' argument means to sort in reverse order."
  :motion mark-whole-buffer
  :move-point nil
  (interactive "<r><a><!>")
  (let ((beg (copy-marker beg))
        (end (copy-marker end))
        sort-fold-case uniq)
    (dolist (opt (append options nil))
      (cond
       ((eq opt ?i) (setq sort-fold-case t))
       ((eq opt ?u) (setq uniq t))
       (t (user-error "Unsupported sort option: %c" opt))))
    (sort-lines reverse beg end)
    (when uniq
      (let (line prev-line)
        (goto-char beg)
        (while (and (< (point) end) (not (eobp)))
          (setq line (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position)))
          (if (and (stringp prev-line)
                   (eq t (compare-strings line nil nil
                                          prev-line nil nil
                                          sort-fold-case)))
              (delete-region (progn (forward-line 0) (point))
                             (progn (forward-line 1) (point)))
            (setq prev-line line)
            (forward-line 1)))))
    (goto-char beg)
    (set-marker beg nil)
    (set-marker end nil)))

;;; Window navigation

(defun vimp-resize-window (new-size &optional horizontal)
  "Set the current window's width or height to NEW-SIZE.
If HORIZONTAL is non-nil the width of the window is changed,
otherwise its height is changed."
  (let ((count (- new-size (if horizontal (window-width) (window-height)))))
    (if (>= emacs-major-version 24)
        (enlarge-window count horizontal)
      (let ((wincfg (current-window-configuration))
            (nwins (length (window-list)))
            (inhibit-redisplay t))
        (catch 'done
          (save-window-excursion
            (while (not (zerop count))
              (if (> count 0)
                  (progn
                    (enlarge-window 1 horizontal)
                    (setq count (1- count)))
                (progn
                  (shrink-window 1 horizontal)
                  (setq count (1+ count))))
              (if (= nwins (length (window-list)))
                  (setq wincfg (current-window-configuration))
                (throw 'done t)))))
        (set-window-configuration wincfg)))))

(defun vimp-get-buffer-tree (wintree)
  "Extracts the buffer tree from a given window tree WINTREE."
  (if (consp wintree)
      (cons (car wintree) (mapcar #'vimp-get-buffer-tree (cddr wintree)))
    (window-buffer wintree)))

(defun vimp-restore-window-tree (win tree)
  "Restore the given buffer-tree layout as subwindows of WIN.
TREE is the tree layout to be restored."
  (cond
   ((and (consp tree) (cddr tree))
    (let ((newwin (split-window win nil (not (car tree)))))
      (vimp-restore-window-tree win (cadr tree))
      (vimp-restore-window-tree newwin (cons (car tree) (cddr tree)))))
   ((consp tree)
    (set-window-buffer win (cadr tree)))
   (t
    (set-window-buffer win tree))))

(defun vimp-alternate-buffer (&optional window)
  "Return the last buffer WINDOW has displayed other than the
current one (equivalent to Vim's alternate buffer).

Returns the first item in `window-prev-buffers' that isn't
`window-buffer' of WINDOW."
  ;; If the last buffer visitied has been killed, then `window-prev-buffers'
  ;; returns a list with `current-buffer' at the head, we account for this
  ;; possibility.
  (let* ((prev-buffers (window-prev-buffers))
         (head (car prev-buffers)))
    (if (eq (car head) (window-buffer window))
        (cadr prev-buffers)
      head)))

(vimp-define-command vimp-switch-to-windows-last-buffer ()
  "Switch to current windows last open buffer."
  :repeat nil
  (let ((previous-place (vimp-alternate-buffer)))
    (when previous-place
      (switch-to-buffer (car previous-place))
      (goto-char (car (last previous-place))))))

(vimp-define-command vimp-window-delete ()
  "Deletes the current window.
If `vimp-auto-balance-windows' is non-nil then all children of
the deleted window's parent window are rebalanced."
  (let ((p (window-parent)))
    (delete-window)
    (when vimp-auto-balance-windows
      ;; balance-windows raises an error if the parent does not have
      ;; any futher childs (then rebalancing is not necessary anywa)
      (condition-case nil
          (balance-windows p)
        (error)))))

(vimp-define-command vimp-window-split (&optional count file)
  "Splits the current window horizontally, COUNT lines height,
editing a certain FILE. The new window will be created below
when `vimp-split-window-below' is non-nil. If COUNT and
`vimp-auto-balance-windows' are both non-nil then all children
of the parent of the splitted window are rebalanced."
  :repeat nil
  (interactive "P<f>")
  (split-window (selected-window) count
                (if vimp-split-window-below 'above 'below))
  (when (and (not count) vimp-auto-balance-windows)
    (balance-windows (window-parent)))
  (when file
    (vimp-edit file)))

(vimp-define-command vimp-window-vsplit (&optional count file)
  "Splits the current window vertically, COUNT columns width,
editing a certain FILE. The new window will be created to the
right when `vimp-vsplit-window-right' is non-nil. If COUNT and
`vimp-auto-balance-windows'are both non-nil then all children
of the parent of the splitted window are rebalanced."
  :repeat nil
  (interactive "P<f>")
  (split-window (selected-window) count
                (if vimp-vsplit-window-right 'left 'right))
  (when (and (not count) vimp-auto-balance-windows)
    (balance-windows (window-parent)))
  (when file
    (vimp-edit file)))

(vimp-define-command vimp-split-buffer (buffer)
  "Splits window and switches to another buffer."
  :repeat nil
  (interactive "<b>")
  (vimp-window-split)
  (vimp-buffer buffer))

(vimp-define-command vimp-split-next-buffer (&optional count)
  "Splits the window and goes to the COUNT-th next buffer in the buffer list."
  :repeat nil
  (interactive "p")
  (vimp-window-split)
  (vimp-next-buffer count))

(vimp-define-command vimp-split-prev-buffer (&optional count)
  "Splits window and goes to the COUNT-th prev buffer in the buffer list."
  :repeat nil
  (interactive "p")
  (vimp-window-split)
  (vimp-prev-buffer count))

(vimp-define-command vimp-window-left (count)
  "Move the cursor to new COUNT-th window left of the current one."
  :repeat nil
  (interactive "p")
  (dotimes (i count)
    (windmove-left)))

(vimp-define-command vimp-window-right (count)
  "Move the cursor to new COUNT-th window right of the current one."
  :repeat nil
  (interactive "p")
  (dotimes (i count)
    (windmove-right)))

(vimp-define-command vimp-window-up (count)
  "Move the cursor to new COUNT-th window above the current one."
  :repeat nil
  (interactive "p")
  (dotimes (i (or count 1))
    (windmove-up)))

(vimp-define-command vimp-window-down (count)
  "Move the cursor to new COUNT-th window below the current one."
  :repeat nil
  (interactive "p")
  (dotimes (i (or count 1))
    (windmove-down)))

(vimp-define-command vimp-window-bottom-right ()
  "Move the cursor to bottom-right window."
  :repeat nil
  (select-window
   (let ((last-sibling (frame-root-window)))
     (while (not (window-live-p last-sibling))
       (setq last-sibling (window-last-child last-sibling)))
     last-sibling)))

(vimp-define-command vimp-window-top-left ()
  "Move the cursor to top-left window."
  :repeat nil
  (select-window
   (let ((first-child (window-child (frame-root-window))))
     (while (not (window-live-p first-child))
       (setq first-child (window-child first-child)))
     first-child)))

(vimp-define-command vimp-window-mru ()
  "Move the cursor to the previous (last accessed) buffer in another window.
More precisely, it selectes the most recently used buffer that is
shown in some other window, preferably of the current frame, and
is different from the current one."
  :repeat nil
  (catch 'done
    (dolist (buf (buffer-list (selected-frame)))
      (let ((win (get-buffer-window buf)))
        (when (and (not (eq buf (current-buffer)))
                   win
                   (not (eq win (selected-window))))
          (select-window win)
          (throw 'done nil))))))

(vimp-define-command vimp-window-next (count)
  "Move the cursor to the next window in the cyclic order.
With COUNT go to the count-th window in the order starting from
top-left."
  :repeat nil
  (interactive "P")
  (if (not count)
      (select-window (next-window))
    (vimp-window-top-left)
    (other-window (1- count))))

(vimp-define-command vimp-window-prev (count)
  "Move the cursor to the previous window in the cyclic order.
With COUNT go to the count-th window in the order starting from
top-left."
  :repeat nil
  (interactive "P")
  (if (not count)
      (select-window (previous-window))
    (vimp-window-top-left)
    (other-window (1- count))))

(vimp-define-command vimp-window-new (count file)
  "Splits the current window horizontally
and opens a new buffer or edits a certain FILE."
  :repeat nil
  (interactive "P<f>")
  (split-window (selected-window) count
                (if vimp-split-window-below 'above 'below))
  (when (and (not count) vimp-auto-balance-windows)
    (balance-windows (window-parent)))
  (if file
      (vimp-edit file)
    (let ((buffer (generate-new-buffer "*new*")))
      (set-window-buffer (selected-window) buffer)
      (with-current-buffer buffer
        (funcall (default-value 'major-mode))))))

(vimp-define-command vimp-window-vnew (count file)
  "Splits the current window vertically
and opens a new buffer name or edits a certain FILE."
  :repeat nil
  (interactive "P<f>")
  (split-window (selected-window) count
                (if vimp-vsplit-window-right 'left 'right))
  (when (and (not count) vimp-auto-balance-windows)
    (balance-windows (window-parent)))
  (if file
      (vimp-edit file)
    (let ((buffer (generate-new-buffer "*new*")))
      (set-window-buffer (selected-window) buffer)
      (with-current-buffer buffer
        (funcall (default-value 'major-mode))))))

(vimp-define-command vimp-window-increase-height (count)
  "Increase current window height by COUNT."
  :repeat nil
  (interactive "p")
  (vimp-resize-window (+ (window-height) count)))

(vimp-define-command vimp-window-decrease-height (count)
  "Decrease current window height by COUNT."
  :repeat nil
  (interactive "p")
  (vimp-resize-window (- (window-height) count)))

(vimp-define-command vimp-window-increase-width (count)
  "Increase current window width by COUNT."
  :repeat nil
  (interactive "p")
  (vimp-resize-window (+ (window-width) count) t))

(vimp-define-command vimp-window-decrease-width (count)
  "Decrease current window width by COUNT."
  :repeat nil
  (interactive "p")
  (vimp-resize-window (- (window-width) count) t))

(vimp-define-command vimp-window-set-height (count)
  "Sets the height of the current window to COUNT."
  :repeat nil
  (interactive "P")
  (vimp-resize-window (or count (frame-height)) nil))

(vimp-define-command vimp-window-set-width (count)
  "Sets the width of the current window to COUNT."
  :repeat nil
  (interactive "P")
  (vimp-resize-window (or count (frame-width)) t))

(vimp-define-command vimp-ex-resize (arg)
  "The ex :resize command.

If ARG is a signed positive integer, increase the current window
height by ARG.

If ARG is a signed negative integer, decrease the current window
height by ARG.

If ARG is a positive integer without explicit sign, set the current
window height to ARG.

If ARG is empty, maximize the current window height."
  (interactive "<a>")
  (if (or (not arg) (= 0 (length arg)))
      (vimp-window-set-height nil)
    (let ((n (string-to-number arg)))
      (if (> n 0)
          (if (= ?+ (aref arg 0))
              (vimp-window-increase-height n)
            (vimp-window-set-height n))
        (vimp-window-decrease-height (- n))))))

(vimp-define-command vimp-window-rotate-upwards ()
  "Rotates the windows according to the currenty cyclic ordering."
  :repeat nil
  (let ((wlist (window-list))
        (blist (mapcar #'(lambda (w) (window-buffer w))
                       (window-list))))
    (setq blist (append (cdr blist) (list (car blist))))
    (while (and wlist blist)
      (set-window-buffer (car wlist) (car blist))
      (setq wlist (cdr wlist)
            blist (cdr blist)))
    (select-window (car (last (window-list))))))

(vimp-define-command vimp-window-rotate-downwards ()
  "Rotates the windows according to the currenty cyclic ordering."
  :repeat nil
  (let ((wlist (window-list))
        (blist (mapcar #'(lambda (w) (window-buffer w))
                       (window-list))))
    (setq blist (append (last blist) blist))
    (while (and wlist blist)
      (set-window-buffer (car wlist) (car blist))
      (setq wlist (cdr wlist)
            blist (cdr blist)))
    (select-window (cadr (window-list)))))

(vimp-define-command vimp-window-move-very-top ()
  "Closes the current window, splits the upper-left one horizontally
and redisplays the current buffer there."
  :repeat nil
  (unless (one-window-p)
    (save-excursion
      (let ((b (current-buffer)))
        (delete-window)
        (let ((btree (vimp-get-buffer-tree (car (window-tree)))))
          (delete-other-windows)
          (let ((newwin (selected-window))
                (subwin (split-window)))
            (vimp-restore-window-tree subwin btree)
            (set-window-buffer newwin b)
            (select-window newwin)))))
    (balance-windows)))

(vimp-define-command vimp-window-move-far-left ()
  "Closes the current window, splits the upper-left one vertically
and redisplays the current buffer there."
  :repeat nil
  (unless (one-window-p)
    (save-excursion
      (let ((b (current-buffer)))
        (delete-window)
        (let ((btree (vimp-get-buffer-tree (car (window-tree)))))
          (delete-other-windows)
          (let ((newwin (selected-window))
                (subwin (split-window-horizontally)))
            (vimp-restore-window-tree subwin btree)
            (set-window-buffer newwin b)
            (select-window newwin)))))
    (balance-windows)))

(vimp-define-command vimp-window-move-far-right ()
  "Closes the current window, splits the lower-right one vertically
and redisplays the current buffer there."
  :repeat nil
  (unless (one-window-p)
    (save-excursion
      (let ((b (current-buffer)))
        (delete-window)
        (let ((btree (vimp-get-buffer-tree (car (window-tree)))))
          (delete-other-windows)
          (let ((subwin (selected-window))
                (newwin (split-window-horizontally)))
            (vimp-restore-window-tree subwin btree)
            (set-window-buffer newwin b)
            (select-window newwin)))))
    (balance-windows)))

(vimp-define-command vimp-window-move-very-bottom ()
  "Closes the current window, splits the lower-right one horizontally
and redisplays the current buffer there."
  :repeat nil
  (unless (one-window-p)
    (save-excursion
      (let ((b (current-buffer)))
        (delete-window)
        (let ((btree (vimp-get-buffer-tree (car (window-tree)))))
          (delete-other-windows)
          (let ((subwin (selected-window))
                (newwin (split-window)))
            (vimp-restore-window-tree subwin btree)
            (set-window-buffer newwin b)
            (select-window newwin)))))
    (balance-windows)))

;;; Mouse handling

;; Large parts of this code are taken from mouse.el which is
;; distributed with GNU Emacs
(defun vimp-mouse-drag-region (start-event)
  "Set the region to the text that the mouse is dragged over.
Highlight the drag area as you move the mouse.
This must be bound to a button-down mouse event.

If the click is in the echo area, display the `*Messages*' buffer.

START-EVENT should be the event that started the drag."
  (interactive "e")
  ;; Give temporary modes such as isearch a chance to turn off.
  (run-hooks 'mouse-leave-buffer-hook)
  (vimp-mouse-drag-track start-event t))
(vimp-set-command-property 'vimp-mouse-drag-region :keep-visual t)

(defun vimp-mouse-drag-track (start-event &optional
                                          do-mouse-drag-region-post-process)
  "Track mouse drags by highlighting area between point and cursor.
The region will be defined with mark and point.
DO-MOUSE-DRAG-REGION-POST-PROCESS should only be used by
`mouse-drag-region'."
  (mouse-minibuffer-check start-event)
  (setq mouse-selection-click-count-buffer (current-buffer))
  (deactivate-mark)
  (let* ((scroll-margin 0) ; Avoid margin scrolling (Bug#9541).
         (original-window (selected-window))
         ;; We've recorded what we needed from the current buffer and
         ;; window, now let's jump to the place of the event, where things
         ;; are happening.
         (_ (mouse-set-point start-event))
         (echo-keystrokes 0)
         (start-posn (event-start start-event))
         (start-point (posn-point start-posn))
         (start-window (posn-window start-posn))
         (start-window-start (window-start start-window))
         (start-hscroll (window-hscroll start-window))
         (bounds (window-edges start-window))
         (make-cursor-line-fully-visible nil)
         (top (nth 1 bounds))
         (bottom (if (window-minibuffer-p start-window)
                     (nth 3 bounds)
                   ;; Don't count the mode line.
                   (1- (nth 3 bounds))))
         (on-link (and mouse-1-click-follows-link
                       (or mouse-1-click-in-non-selected-windows
                           (eq start-window original-window))
                       ;; Use start-point before the intangibility
                       ;; treatment, in case we click on a link inside an
                       ;; intangible text.
                       (mouse-on-link-p start-posn)))
         (click-count (1- (event-click-count start-event)))
         (remap-double-click (and on-link
                                  (eq mouse-1-click-follows-link 'double)
                                  (= click-count 1)))
         ;; Suppress automatic hscrolling, because that is a nuisance
         ;; when setting point near the right fringe (but see below).
         (auto-hscroll-mode-saved auto-hscroll-mode)
         (auto-hscroll-mode nil)
         event end end-point)

    (setq mouse-selection-click-count click-count)
    ;; In case the down click is in the middle of some intangible text,
    ;; use the end of that text, and put it in START-POINT.
    (if (< (point) start-point)
        (goto-char start-point))
    (setq start-point (point))
    (if remap-double-click
        (setq click-count 0))

    (setq click-count (mod click-count 4))

    ;; activate correct visual state
    (let ((range (vimp-mouse-start-end start-point start-point click-count)))
      (set-mark (nth 0 range))
      (goto-char (nth 1 range)))

    (cond
     ((= click-count 0)
      (when (vimp-visual-state-p) (vimp-exit-visual-state)))
     ((= click-count 1)
      (vimp-visual-char)
      (vimp-visual-post-command))
     ((= click-count 2)
      (vimp-visual-line)
      (vimp-visual-post-command))
     ((= click-count 3)
      (vimp-visual-block)
      (vimp-visual-post-command)))

    ;; Track the mouse until we get a non-movement event.
    (track-mouse
      (while (progn
               (setq event (read-event))
               (or (mouse-movement-p event)
                   (memq (car-safe event) '(switch-frame select-window))))
        (unless (vimp-visual-state-p)
          (cond
           ((= click-count 0) (vimp-visual-char))
           ((= click-count 1) (vimp-visual-char))
           ((= click-count 2) (vimp-visual-line))
           ((= click-count 3) (vimp-visual-block))))

        (vimp-visual-pre-command)
        (unless (memq (car-safe event) '(switch-frame select-window))
          ;; Automatic hscrolling did not occur during the call to
          ;; `read-event'; but if the user subsequently drags the
          ;; mouse, go ahead and hscroll.
          (let ((auto-hscroll-mode auto-hscroll-mode-saved))
            (redisplay))
          (setq end (event-end event)
                end-point (posn-point end))
          (if (and (eq (posn-window end) start-window)
                   (integer-or-marker-p end-point))
              (vimp-mouse--drag-set-mark-and-point start-point
                                                   end-point click-count)
            (let ((mouse-row (cdr (cdr (mouse-position)))))
              (cond
               ((null mouse-row))
               ((< mouse-row top)
                (mouse-scroll-subr start-window (- mouse-row top)
                                   nil start-point))
               ((>= mouse-row bottom)
                (mouse-scroll-subr start-window (1+ (- mouse-row bottom))
                                   nil start-point))))))
        (vimp-visual-post-command)))

    ;; Handle the terminating event if possible.
    (when (consp event)
      ;; Ensure that point is on the end of the last event.
      (when (and (setq end-point (posn-point (event-end event)))
                 (eq (posn-window end) start-window)
                 (integer-or-marker-p end-point)
                 (/= start-point end-point))
        (vimp-mouse--drag-set-mark-and-point start-point
                                             end-point click-count))

      ;; Find its binding.
      (let* ((fun (key-binding (vector (car event))))
             (do-multi-click (and (> (event-click-count event) 0)
                                  (functionp fun)
                                  (not (memq fun '(mouse-set-point
                                                   mouse-set-region))))))
        (if (and (or (/= (mark) (point))
                     (= click-count 1) ; word selection
                     (and (memq (vimp-visual-type) '(line block))))
                 (not do-multi-click))

            ;; If point has moved, finish the drag.
            (let (last-command this-command)
              (and mouse-drag-copy-region
                   do-mouse-drag-region-post-process
                   (let (deactivate-mark)
                     (vimp-visual-expand-region)
                     (copy-region-as-kill (mark) (point))
                     (vimp-visual-contract-region))))

          ;; If point hasn't moved, run the binding of the
          ;; terminating up-event.
          (if do-multi-click
              (goto-char start-point)
            (deactivate-mark))
          (when (and (functionp fun)
                     (= start-hscroll (window-hscroll start-window))
                     ;; Don't run the up-event handler if the window
                     ;; start changed in a redisplay after the
                     ;; mouse-set-point for the down-mouse event at
                     ;; the beginning of this function.  When the
                     ;; window start has changed, the up-mouse event
                     ;; contains a different position due to the new
                     ;; window contents, and point is set again.
                     (or end-point
                         (= (window-start start-window)
                            start-window-start)))
            (when (and on-link
                       (= start-point (point))
                       (vimp-mouse--remap-link-click-p start-event event))
              ;; If we rebind to mouse-2, reselect previous selected
              ;; window, so that the mouse-2 event runs in the same
              ;; situation as if user had clicked it directly.  Fixes
              ;; the bug reported by juri@jurta.org on 2005-12-27.
              (if (or (vectorp on-link) (stringp on-link))
                  (setq event (aref on-link 0))
                (select-window original-window)
                (setcar event 'mouse-2)
                ;; If this mouse click has never been done by the
                ;; user, it doesn't have the necessary property to be
                ;; interpreted correctly.
                (put 'mouse-2 'event-kind 'mouse-click)))
            (push event unread-command-events)))))))

;; This function is a plain copy of `mouse--drag-set-mark-and-point',
;; which is only available in Emacs 24
(defun vimp-mouse--drag-set-mark-and-point (start click click-count)
  (let* ((range (vimp-mouse-start-end start click click-count))
         (beg (nth 0 range))
         (end (nth 1 range)))
    (cond ((eq (mark) beg)
           (goto-char end))
          ((eq (mark) end)
           (goto-char beg))
          ((< click (mark))
           (set-mark end)
           (goto-char beg))
          (t
           (set-mark beg)
           (goto-char end)))))

;; This function is a plain copy of `mouse--remap-link-click-p',
;; which is only available in Emacs 23
(defun vimp-mouse--remap-link-click-p (start-event end-event)
  (or (and (eq mouse-1-click-follows-link 'double)
           (= (event-click-count start-event) 2))
      (and
       (not (eq mouse-1-click-follows-link 'double))
       (= (event-click-count start-event) 1)
       (= (event-click-count end-event) 1)
       (or (not (integerp mouse-1-click-follows-link))
           (let ((t0 (posn-timestamp (event-start start-event)))
                 (t1 (posn-timestamp (event-end   end-event))))
             (and (integerp t0) (integerp t1)
                  (if (> mouse-1-click-follows-link 0)
                      (<= (- t1 t0) mouse-1-click-follows-link)
                    (< (- t0 t1) mouse-1-click-follows-link))))))))

(defun vimp-mouse-start-end (start end mode)
  "Return a list of region bounds based on START and END according to MODE.
If MODE is not 1 then set point to (min START END), mark to (max
START END).  If MODE is 1 then set point to start of word at (min
START END), mark to end of word at (max START END)."
  (vimp-sort start end)
  (setq mode (mod mode 4))
  (if (/= mode 1) (list start end)
    (list
     (save-excursion
       (goto-char (min (point-max) (1+ start)))
       (if (zerop (forward-thing vimp-mouse-word -1))
           (let ((bpnt (point)))
             (forward-thing vimp-mouse-word +1)
             (if (> (point) start) bpnt (point)))
         (point-min)))
     (save-excursion
       (goto-char end)
       (1-
        (if (zerop (forward-thing vimp-mouse-word +1))
            (let ((epnt (point)))
              (forward-thing vimp-mouse-word -1)
              (if (<= (point) end) epnt (point)))
          (point-max)))))))

;;; State switching

(vimp-define-command vimp-exit-emacs-state (&optional buffer message)
  "Exit Emacs state.
Changes the state to the previous state, or to Normal state
if the previous state was Emacs state."
  :keep-visual t
  :suppress-operator t
  (interactive '(nil t))
  (with-current-buffer (or buffer (current-buffer))
    (when (vimp-emacs-state-p)
      (vimp-change-to-previous-state buffer message)
      (when (vimp-emacs-state-p)
        (vimp-normal-state (and message 1))))))

(defun vimp-execute-in-normal-state ()
  "Execute the next command in Normal state."
  (interactive)
  (vimp-delay '(not (memq this-command
                          '(vimp-execute-in-normal-state
                            vimp-use-register
                            digit-argument
                            negative-argument
                            universal-argument
                            universal-argument-minus
                            universal-argument-more
                            universal-argument-other-key)))
      `(progn
         (vimp-change-state ',vimp-state)
         (setq vimp-move-cursor-back ',vimp-move-cursor-back))
    'post-command-hook)
  (setq vimp-move-cursor-back nil)
  (vimp-normal-state)
  (vimp-echo "Switched to Normal state for the next command ..."))

(defun vimp-stop-execute-in-emacs-state ()
  (when (and (not (eq this-command #'vimp-execute-in-emacs-state))
             (not (minibufferp)))
    (remove-hook 'post-command-hook 'vimp-stop-execute-in-emacs-state)
    (when (buffer-live-p vimp-execute-in-emacs-state-buffer)
      (with-current-buffer vimp-execute-in-emacs-state-buffer
        (if (and (eq vimp-previous-state 'visual)
                 (not (use-region-p)))
            (progn
              (vimp-change-to-previous-state)
              (vimp-exit-visual-state))
          (vimp-change-to-previous-state))))
    (setq vimp-execute-in-emacs-state-buffer nil)))

(vimp-define-command vimp-execute-in-emacs-state ()
  "Execute the next command in Emacs state."
  (add-hook 'post-command-hook #'vimp-stop-execute-in-emacs-state t)
  (setq vimp-execute-in-emacs-state-buffer (current-buffer))
  (cond
   ((vimp-visual-state-p)
    (let ((mrk (mark))
          (pnt (point)))
      (vimp-emacs-state)
      (set-mark mrk)
      (goto-char pnt)))
   (t
    (vimp-emacs-state)))
  (vimp-echo "Switched to Emacs state for the next command ..."))

(defun vimp-exit-visual-and-repeat (event)
  "Exit insert state and repeat event.
This special command should be used if some command called from
visual state should actually be called in normal-state.  The main
reason for doing this is that the repeat system should *not*
record the visual state information for some command.  This
command should be bound to exactly the same event in visual state
as the original command is bound in normal state.  EVENT is the
event that triggered the execution of this command."
  (interactive "e")
  (when (vimp-visual-state-p)
    (vimp-exit-visual-state)
    (push event unread-command-events)))
(vimp-declare-ignore-repeat 'vimp-exit-visual-and-repeat)

(provide 'vimp-commands)

;;; vimp-commands.el ends here
