;;; vimp-types.el --- Type system

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

;;; Commentary:

;; A type defines a transformation on a pair of buffer positions.
;; Types are used by Visual state (character/line/block selection)
;; and Operator-Pending state (character/line/block motions).
;;
;; The basic transformation is "expansion". For example, the `line'
;; type "expands" a pair of positions to whole lines by moving the
;; first position to the beginning of the line and the last position
;; to the end of the line. That expanded selection is what the rest
;; of Emacs sees and acts on.
;;
;; An optional transformation is "contraction", which is the opposite
;; of expansion. If the transformation is one-to-one, expansion
;; followed by contraction always returns the original range.
;; (The `line' type is not one-to-one, as it may expand multiple
;; positions to the same lines.)
;;
;; Another optional transformation is "normalization", which takes
;; two unexpanded positions and adjusts them before expansion.
;; This is useful for cleaning up "invalid" positions.
;;
;; Types are defined at the end of this file using the macro
;; `vimp-define-type'.

(require 'vimp-common)
(require 'vimp-macros)

;;; Code:

;;; Type definitions

(vimp-define-type exclusive
  "Return the positions unchanged, with some exceptions.
If the end position is at the beginning of a line, then:

* If the beginning position is at or before the first non-blank
  character on the line, return `line' (expanded).

* Otherwise, move the end position to the end of the previous
  line and return `inclusive' (expanded)."
  :normalize (lambda (beg end)
               (cond
                ((progn
                   (goto-char end)
                   (and (/= beg end) (bolp)))
                 (setq end (max beg (1- end)))
                 (cond
                  ((progn
                     (goto-char beg)
                     (looking-back "^[ \f\t\v]*" (line-beginning-position)))
                   (vimp-expand beg end 'line))
                  (t
                   (unless vimp-cross-lines
                     (setq end (max beg (1- end))))
                   (vimp-expand beg end 'inclusive))))
                (t
                 (vimp-range beg end))))
  :string (lambda (beg end)
            (let ((width (- end beg)))
              (format "%s character%s" width
                      (if (= width 1) "" "s")))))

(vimp-define-type inclusive
  "Include the character under point.
If the end position is at the beginning of a line or the end of a
line and `vimp-want-visual-char-semi-exclusive', then:

* If in visual state return `exclusive' (expanded)."
  :expand (lambda (beg end)
            (if (and vimp-want-visual-char-semi-exclusive
                     (vimp-visual-state-p)
                     (< beg end)
                     (save-excursion
                       (goto-char end)
                       (or (bolp) (eolp))))
                (vimp-range beg end 'exclusive)
              (vimp-range beg (1+ end))))
  :contract (lambda (beg end)
              (vimp-range beg (max beg (1- end))))
  :normalize (lambda (beg end)
               (goto-char end)
               (when (eq (char-after) ?\n)
                 (setq end (max beg (1- end))))
               (vimp-range beg end))
  :string (lambda (beg end)
            (let ((width (- end beg)))
              (format "%s character%s" width
                      (if (= width 1) "" "s")))))

(vimp-define-type line
  "Include whole lines."
  :one-to-one nil
  :expand (lambda (beg end)
            (vimp-range
             (progn
               (goto-char beg)
               (min (line-beginning-position)
                    (progn
                      ;; move to beginning of line as displayed
                      (vimp-move-beginning-of-line)
                      (line-beginning-position))))
             (progn
               (goto-char end)
               (max (line-beginning-position 2)
                    (progn
                      ;; move to end of line as displayed
                      (vimp-move-end-of-line)
                      (line-beginning-position 2))))))
  :contract (lambda (beg end)
              (vimp-range beg (max beg (1- end))))
  :string (lambda (beg end)
            (let ((height (count-lines beg end)))
              (format "%s line%s" height
                      (if (= height 1) "" "s")))))

(vimp-define-type block
  "Like `inclusive', but for rectangles:
the last column is included."
  :expand (lambda (beg end &rest properties)
            (let ((beg-col (vimp-column beg))
                  (end-col (vimp-column end))
                  (corner (plist-get properties :corner)))
              ;; Since blocks are implemented as a pair of buffer
              ;; positions, expansion is restricted to what the buffer
              ;; allows. In the case of a one-column block, there are
              ;; two ways to expand it (either move the upper corner
              ;; beyond the lower corner, or the lower beyond the
              ;; upper), so try out both possibilities when
              ;; encountering the end of the line.
              (cond
               ((= beg-col end-col)
                (goto-char end)
                (cond
                 ((eolp)
                  (goto-char beg)
                  (if (eolp)
                      (vimp-range beg end)
                    (vimp-range (1+ beg) end)))
                 ((memq corner '(lower-right upper-right right))
                  (vimp-range (1+ beg) end))
                 (t
                  (vimp-range beg (1+ end)))))
               ((< beg-col end-col)
                (goto-char end)
                (if (eolp)
                    (vimp-range beg end)
                  (vimp-range beg (1+ end))))
               (t
                (goto-char beg)
                (if (eolp)
                    (vimp-range beg end)
                  (vimp-range (1+ beg) end))))))
  :contract (lambda (beg end)
              (let ((beg-col (vimp-column beg))
                    (end-col (vimp-column end)))
                (if (> beg-col end-col)
                    (vimp-range (1- beg) end)
                  (vimp-range beg (max beg (1- end))))))
  :string (lambda (beg end)
            (let ((height (count-lines
                           beg
                           (progn
                             (goto-char end)
                             (if (and (bolp) (not (eobp)))
                                 (1+ end)
                               end))))
                  (width (abs (- (vimp-column beg)
                                 (vimp-column end)))))
              (format "%s row%s and %s column%s"
                      height
                      (if (= height 1) "" "s")
                      width
                      (if (= width 1) "" "s"))))
  :rotate (lambda (beg end &rest properties)
            "Rotate block according to :corner property.
:corner can be one of `upper-left',``upper-right', `lower-left'
and `lower-right'."
            (let ((left  (vimp-column beg))
                  (right (vimp-column end))
                  (corner (or (plist-get properties :corner)
                              'upper-left)))
              (vimp-sort left right)
              (goto-char beg)
              (if (memq corner '(upper-right lower-left))
                  (move-to-column right)
                (move-to-column left))
              (setq beg (point))
              (goto-char end)
              (if (memq corner '(upper-right lower-left))
                  (move-to-column left)
                (move-to-column right))
              (setq end (point))
              (setq properties (plist-put properties
                                          :corner corner))
              (apply #'vimp-range beg end properties))))

(vimp-define-type rectangle
  "Like `exclusive', but for rectangles:
the last column is excluded."
  :expand (lambda (beg end)
            ;; select at least one column
            (if (= (vimp-column beg) (vimp-column end))
                (vimp-expand beg end 'block)
              (vimp-range beg end 'block))))

;;; Standard interactive codes

(vimp-define-interactive-code "*"
  "Signal error if the buffer is read-only."
  (when buffer-read-only
    (signal 'buffer-read-only nil)))

(vimp-define-interactive-code "b" (prompt)
  "Name of existing buffer."
  (list (read-buffer prompt (current-buffer) t)))

(vimp-define-interactive-code "c"
  "Read character."
  (list (read-char)))

(vimp-define-interactive-code "p"
  "Prefix argument converted to number."
  (list (prefix-numeric-value current-prefix-arg)))

(vimp-define-interactive-code "P"
  "Prefix argument in raw form."
  (list current-prefix-arg))

;;; Custom interactive codes

(vimp-define-interactive-code "<c>"
  "Count."
  (list (when current-prefix-arg
          (prefix-numeric-value
           current-prefix-arg))))

(vimp-define-interactive-code "<vc>"
  "Count, but only in visual state.
This should be used by an operator taking a count. In normal
state the count should not be handled by the operator but by the
motion that defines the operator's range. In visual state the
range is specified by the visual region and the count is not used
at all. Thus in the case the operator may use the count
directly."
  (list (when (and (vimp-visual-state-p) current-prefix-arg)
          (prefix-numeric-value
           current-prefix-arg))))

(vimp-define-interactive-code "<C>"
  "Character read through `vimp-read-key'."
  (list
   (if (vimp-operator-state-p)
       (vimp-without-restriction (vimp-read-key))
     (vimp-read-key))))

(vimp-define-interactive-code "<r>"
  "Untyped motion range (BEG END)."
  (vimp-operator-range))

(vimp-define-interactive-code "<R>"
  "Typed motion range (BEG END TYPE)."
  (vimp-operator-range t))

(vimp-define-interactive-code "<v>"
  "Typed motion range of visual range(BEG END TYPE).
If visual state is inactive then those values are nil."
  (if (vimp-visual-state-p)
      (let ((range (vimp-visual-range)))
        (list (car range)
              (cadr range)
              (vimp-type range)))
    (list nil nil nil)))

(vimp-define-interactive-code "<x>"
  "Current register."
  (list vimp-this-register))

(vimp-define-interactive-code "<y>"
  "Current yank-handler."
  (list (vimp-yank-handler)))

(vimp-define-interactive-code "<a>"
  "Ex argument."
  :ex-arg t
  (list (when (vimp-ex-p) vimp-ex-argument)))

(vimp-define-interactive-code "<f>"
  "Ex file argument."
  :ex-arg file
  (list (when (vimp-ex-p) (vimp-ex-file-arg))))

(vimp-define-interactive-code "<b>"
  "Ex buffer argument."
  :ex-arg buffer
  (list (when (vimp-ex-p) vimp-ex-argument)))

(vimp-define-interactive-code "<sh>"
  "Ex shell command argument."
  :ex-arg shell
  (list (when (vimp-ex-p) vimp-ex-argument)))

(vimp-define-interactive-code "<fsh>"
  "Ex file or shell command argument."
  :ex-arg file-or-shell
  (list (when (vimp-ex-p) vimp-ex-argument)))

(vimp-define-interactive-code "<sym>"
  "Ex symbolic argument."
  :ex-arg sym
  (list (when (and (vimp-ex-p) vimp-ex-argument)
          (intern vimp-ex-argument))))

(vimp-define-interactive-code "<addr>"
  "Ex line number."
  (list
   (and (vimp-ex-p)
        (let ((expr (vimp-ex-parse  vimp-ex-argument)))
          (if (eq (car expr) 'vimp-goto-line)
              (save-excursion
                (goto-char vimp-ex-point)
                (eval (cadr expr)))
            (user-error "Invalid address"))))))

(vimp-define-interactive-code "<!>"
  "Ex bang argument."
  :ex-bang t
  (list (when (vimp-ex-p) vimp-ex-bang)))

(vimp-define-interactive-code "</>"
  "Ex delimited argument."
  (when (vimp-ex-p)
    (vimp-delimited-arguments vimp-ex-argument)))

(vimp-define-interactive-code "<g/>"
  "Ex global argument."
  (when (vimp-ex-p)
    (vimp-ex-parse-global vimp-ex-argument)))

(vimp-define-interactive-code "<s/>"
  "Ex substitution argument."
  :ex-arg substitution
  (when (vimp-ex-p)
    (vimp-ex-get-substitute-info vimp-ex-argument t)))

(provide 'vimp-types)

;;; vimp-types.el ends here
