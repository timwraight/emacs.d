;;; vimp-maps.el --- Default keymaps

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

(require 'vimp-states)
(require 'vimp-ex)
(require 'vimp-commands)
(require 'vimp-command-window)

;;; Code:

;;; Normal state

(define-key vimp-normal-state-map "a" 'vimp-append)
(define-key vimp-normal-state-map "A" 'vimp-append-line)
(define-key vimp-normal-state-map "c" 'vimp-change)
(define-key vimp-normal-state-map "C" 'vimp-change-line)
(define-key vimp-normal-state-map "d" 'vimp-delete)
(define-key vimp-normal-state-map "D" 'vimp-delete-line)
(define-key vimp-normal-state-map "i" 'vimp-insert)
(define-key vimp-normal-state-map (kbd "<insert>") 'vimp-insert)
(define-key vimp-normal-state-map (kbd "<insertchar>") 'vimp-insert)
(define-key vimp-normal-state-map "I" 'vimp-insert-line)
(define-key vimp-normal-state-map "J" 'vimp-join)
(define-key vimp-normal-state-map "m" 'vimp-set-marker)
(define-key vimp-normal-state-map "o" 'vimp-open-below)
(define-key vimp-normal-state-map "O" 'vimp-open-above)
(define-key vimp-normal-state-map "p" 'vimp-paste-after)
(define-key vimp-normal-state-map "P" 'vimp-paste-before)
(define-key vimp-normal-state-map "q" 'vimp-record-macro)
(define-key vimp-normal-state-map "r" 'vimp-replace)
(define-key vimp-normal-state-map "R" 'vimp-replace-state)
(define-key vimp-normal-state-map "s" 'vimp-substitute)
(define-key vimp-normal-state-map "S" 'vimp-change-whole-line)
(define-key vimp-normal-state-map "x" 'vimp-delete-char)
(define-key vimp-normal-state-map "X" 'vimp-delete-backward-char)
(define-key vimp-normal-state-map [deletechar] 'vimp-delete-char)
(define-key vimp-normal-state-map "y" 'vimp-yank)
(define-key vimp-normal-state-map "Y" 'vimp-yank-line)
(define-key vimp-normal-state-map "&" 'vimp-ex-repeat-substitute)
(define-key vimp-normal-state-map "g&" 'vimp-ex-repeat-global-substitute)
(define-key vimp-normal-state-map "g8" 'what-cursor-position)
(define-key vimp-normal-state-map "ga" 'what-cursor-position)
(define-key vimp-normal-state-map "gi" 'vimp-insert-resume)
(define-key vimp-normal-state-map "gJ" 'vimp-join-whitespace)
(define-key vimp-normal-state-map "gq" 'vimp-fill-and-move)
(define-key vimp-normal-state-map "gw" 'vimp-fill)
(define-key vimp-normal-state-map "gu" 'vimp-downcase)
(define-key vimp-normal-state-map "gU" 'vimp-upcase)
(define-key vimp-normal-state-map "gf" 'find-file-at-point)
(define-key vimp-normal-state-map "gF" 'vimp-find-file-at-point-with-line)
(define-key vimp-normal-state-map "g?" 'vimp-rot13)
(define-key vimp-normal-state-map "g~" 'vimp-invert-case)
(define-key vimp-normal-state-map "zo" 'vimp-open-fold)
(define-key vimp-normal-state-map "zO" 'vimp-open-fold-rec)
(define-key vimp-normal-state-map "zc" 'vimp-close-fold)
(define-key vimp-normal-state-map "za" 'vimp-toggle-fold)
(define-key vimp-normal-state-map "zr" 'vimp-open-folds)
(define-key vimp-normal-state-map "zm" 'vimp-close-folds)
(define-key vimp-normal-state-map "z=" 'ispell-word)
(define-key vimp-normal-state-map "\C-n" 'vimp-paste-pop-next)
(define-key vimp-normal-state-map "\C-p" 'vimp-paste-pop)
(define-key vimp-normal-state-map "\C-t" 'pop-tag-mark)
(define-key vimp-normal-state-map (kbd "C-.") 'vimp-repeat-pop)
(define-key vimp-normal-state-map (kbd "M-.") 'vimp-repeat-pop-next)
(define-key vimp-normal-state-map "." 'vimp-repeat)
(define-key vimp-normal-state-map "@" 'vimp-execute-macro)
(define-key vimp-normal-state-map "\"" 'vimp-use-register)
(define-key vimp-normal-state-map "~" 'vimp-invert-char)
(define-key vimp-normal-state-map "=" 'vimp-indent)
(define-key vimp-normal-state-map "<" 'vimp-shift-left)
(define-key vimp-normal-state-map ">" 'vimp-shift-right)
(define-key vimp-normal-state-map "ZZ" 'vimp-save-modified-and-close)
(define-key vimp-normal-state-map "ZQ" 'vimp-quit)
(define-key vimp-normal-state-map (kbd "DEL") 'vimp-backward-char)
(define-key vimp-normal-state-map [escape] 'vimp-force-normal-state)
(define-key vimp-normal-state-map [remap cua-paste-pop] 'vimp-paste-pop)
(define-key vimp-normal-state-map [remap yank-pop] 'vimp-paste-pop)

;; go to last change
(define-key vimp-normal-state-map "g;" 'goto-last-change)
(define-key vimp-normal-state-map "g," 'goto-last-change-reverse)

;; undo
(define-key vimp-normal-state-map "u" 'undo)
(define-key vimp-normal-state-map "\C-r" 'redo)

;; window commands
(define-prefix-command 'vimp-window-map)
(define-key vimp-window-map "b" 'vimp-window-bottom-right)
(define-key vimp-window-map "c" 'vimp-window-delete)
(define-key vimp-window-map "h" 'vimp-window-left)
(define-key vimp-window-map "H" 'vimp-window-move-far-left)
(define-key vimp-window-map "j" 'vimp-window-down)
(define-key vimp-window-map "J" 'vimp-window-move-very-bottom)
(define-key vimp-window-map "k" 'vimp-window-up)
(define-key vimp-window-map "K" 'vimp-window-move-very-top)
(define-key vimp-window-map "l" 'vimp-window-right)
(define-key vimp-window-map "L" 'vimp-window-move-far-right)
(define-key vimp-window-map "n" 'vimp-window-new)
(define-key vimp-window-map "o" 'delete-other-windows)
(define-key vimp-window-map "p" 'vimp-window-mru)
(define-key vimp-window-map "r" 'vimp-window-rotate-downwards)
(define-key vimp-window-map "R" 'vimp-window-rotate-upwards)
(define-key vimp-window-map "s" 'vimp-window-split)
(define-key vimp-window-map "S" 'vimp-window-split)
(define-key vimp-window-map "t" 'vimp-window-top-left)
(define-key vimp-window-map "v" 'vimp-window-vsplit)
(define-key vimp-window-map "w" 'vimp-window-next)
(define-key vimp-window-map "W" 'vimp-window-prev)
(define-key vimp-window-map "+" 'vimp-window-increase-height)
(define-key vimp-window-map "-" 'vimp-window-decrease-height)
(define-key vimp-window-map "_" 'vimp-window-set-height)
(define-key vimp-window-map "<" 'vimp-window-decrease-width)
(define-key vimp-window-map ">" 'vimp-window-increase-width)
(define-key vimp-window-map "=" 'balance-windows)
(define-key vimp-window-map "|" 'vimp-window-set-width)
(define-key vimp-window-map "\C-b" 'vimp-window-bottom-right)
(define-key vimp-window-map "\C-c" 'vimp-window-delete)
(define-key vimp-window-map (kbd "C-S-h") 'vimp-window-move-far-left)
(define-key vimp-window-map (kbd "C-S-j") 'vimp-window-move-very-bottom)
(define-key vimp-window-map (kbd "C-S-k") 'vimp-window-move-very-top)
(define-key vimp-window-map (kbd "C-S-l") 'vimp-window-move-far-right)
(define-key vimp-window-map "\C-n" 'vimp-window-new)
(define-key vimp-window-map "\C-o" 'delete-other-windows)
(define-key vimp-window-map "\C-p" 'vimp-window-mru)
(define-key vimp-window-map "\C-r" 'vimp-window-rotate-downwards)
(define-key vimp-window-map (kbd "C-S-r") 'vimp-window-rotate-upwards)
(define-key vimp-window-map "\C-s" 'vimp-window-split)
(define-key vimp-window-map (kbd "C-S-s") 'vimp-window-split)
(define-key vimp-window-map "\C-t" 'vimp-window-top-left)
(define-key vimp-window-map "\C-v" 'vimp-window-vsplit)
(define-key vimp-window-map "\C-w" 'vimp-window-next)
(define-key vimp-window-map (kbd "C-S-W") 'vimp-window-prev)
(define-key vimp-window-map "\C-_" 'vimp-window-set-height)
(define-key vimp-window-map "\C-f" 'ffap-other-window)

;;; Motion state

;; "0" is a special command when called first
(vimp-redirect-digit-argument vimp-motion-state-map "0" 'vimp-beginning-of-line)
(define-key vimp-motion-state-map "1" 'digit-argument)
(define-key vimp-motion-state-map "2" 'digit-argument)
(define-key vimp-motion-state-map "3" 'digit-argument)
(define-key vimp-motion-state-map "4" 'digit-argument)
(define-key vimp-motion-state-map "5" 'digit-argument)
(define-key vimp-motion-state-map "6" 'digit-argument)
(define-key vimp-motion-state-map "7" 'digit-argument)
(define-key vimp-motion-state-map "8" 'digit-argument)
(define-key vimp-motion-state-map "9" 'digit-argument)
(define-key vimp-motion-state-map "b" 'vimp-backward-word-begin)
(define-key vimp-motion-state-map "B" 'vimp-backward-WORD-begin)
(define-key vimp-motion-state-map "e" 'vimp-forward-word-end)
(define-key vimp-motion-state-map "E" 'vimp-forward-WORD-end)
(define-key vimp-motion-state-map "f" 'vimp-find-char)
(define-key vimp-motion-state-map "F" 'vimp-find-char-backward)
(define-key vimp-motion-state-map "G" 'vimp-goto-line)
(define-key vimp-motion-state-map "h" 'vimp-backward-char)
(define-key vimp-motion-state-map "H" 'vimp-window-top)
(define-key vimp-motion-state-map "j" 'vimp-next-line)
(define-key vimp-motion-state-map "k" 'vimp-previous-line)
(define-key vimp-motion-state-map "l" 'vimp-forward-char)
(define-key vimp-motion-state-map " " 'vimp-forward-char)
(define-key vimp-motion-state-map "K" 'vimp-lookup)
(define-key vimp-motion-state-map "L" 'vimp-window-bottom)
(define-key vimp-motion-state-map "M" 'vimp-window-middle)
(define-key vimp-motion-state-map "n" 'vimp-search-next)
(define-key vimp-motion-state-map "N" 'vimp-search-previous)
(define-key vimp-motion-state-map "t" 'vimp-find-char-to)
(define-key vimp-motion-state-map "T" 'vimp-find-char-to-backward)
(define-key vimp-motion-state-map "w" 'vimp-forward-word-begin)
(define-key vimp-motion-state-map "W" 'vimp-forward-WORD-begin)
(define-key vimp-motion-state-map "y" 'vimp-yank)
(define-key vimp-motion-state-map "Y" 'vimp-yank-line)
(define-key vimp-motion-state-map "gd" 'vimp-goto-definition)
(define-key vimp-motion-state-map "ge" 'vimp-backward-word-end)
(define-key vimp-motion-state-map "gE" 'vimp-backward-WORD-end)
(define-key vimp-motion-state-map "gg" 'vimp-goto-first-line)
(define-key vimp-motion-state-map "gj" 'vimp-next-visual-line)
(define-key vimp-motion-state-map "gk" 'vimp-previous-visual-line)
(define-key vimp-motion-state-map "g0" 'vimp-beginning-of-visual-line)
(define-key vimp-motion-state-map "g_" 'vimp-last-non-blank)
(define-key vimp-motion-state-map "g^" 'vimp-first-non-blank-of-visual-line)
(define-key vimp-motion-state-map "gm" 'vimp-middle-of-visual-line)
(define-key vimp-motion-state-map "g$" 'vimp-end-of-visual-line)
(define-key vimp-motion-state-map "g\C-]" 'find-tag)
(define-key vimp-motion-state-map "{" 'vimp-backward-paragraph)
(define-key vimp-motion-state-map "}" 'vimp-forward-paragraph)
(define-key vimp-motion-state-map "#" 'vimp-search-word-backward)
(define-key vimp-motion-state-map "g#" 'vimp-search-unbounded-word-backward)
(define-key vimp-motion-state-map "$" 'vimp-end-of-line)
(define-key vimp-motion-state-map "%" 'vimp-jump-item)
(define-key vimp-motion-state-map "`" 'vimp-goto-mark)
(define-key vimp-motion-state-map "'" 'vimp-goto-mark-line)
(define-key vimp-motion-state-map "(" 'vimp-backward-sentence-begin)
(define-key vimp-motion-state-map ")" 'vimp-forward-sentence-begin)
(define-key vimp-motion-state-map "]]" 'vimp-forward-section-begin)
(define-key vimp-motion-state-map "][" 'vimp-forward-section-end)
(define-key vimp-motion-state-map "[[" 'vimp-backward-section-begin)
(define-key vimp-motion-state-map "[]" 'vimp-backward-section-end)
(define-key vimp-motion-state-map "[(" 'vimp-previous-open-paren)
(define-key vimp-motion-state-map "])" 'vimp-next-close-paren)
(define-key vimp-motion-state-map "[{" 'vimp-previous-open-brace)
(define-key vimp-motion-state-map "]}" 'vimp-next-close-brace)
(define-key vimp-motion-state-map "*" 'vimp-search-word-forward)
(define-key vimp-motion-state-map "g*" 'vimp-search-unbounded-word-forward)
(define-key vimp-motion-state-map "," 'vimp-repeat-find-char-reverse)
(define-key vimp-motion-state-map "/" 'vimp-search-forward)
(define-key vimp-motion-state-map ";" 'vimp-repeat-find-char)
(define-key vimp-motion-state-map "?" 'vimp-search-backward)
(define-key vimp-motion-state-map "|" 'vimp-goto-column)
(define-key vimp-motion-state-map "^" 'vimp-first-non-blank)
(define-key vimp-motion-state-map "+" 'vimp-next-line-first-non-blank)
(define-key vimp-motion-state-map "_" 'vimp-next-line-1-first-non-blank)
(define-key vimp-motion-state-map "-" 'vimp-previous-line-first-non-blank)
(define-key vimp-motion-state-map "\C-w" 'vimp-window-map)
(define-key vimp-motion-state-map (kbd "C-6") 'vimp-switch-to-windows-last-buffer)
(define-key vimp-motion-state-map "\C-]" 'vimp-jump-to-tag)
(define-key vimp-motion-state-map (kbd "C-b") 'vimp-scroll-page-up)
(define-key vimp-motion-state-map (kbd "C-d") 'vimp-scroll-down)
(define-key vimp-motion-state-map (kbd "C-e") 'vimp-scroll-line-down)
(define-key vimp-motion-state-map (kbd "C-f") 'vimp-scroll-page-down)
(define-key vimp-motion-state-map (kbd "C-o") 'vimp-jump-backward)
(define-key vimp-motion-state-map (kbd "C-y") 'vimp-scroll-line-up)
(define-key vimp-motion-state-map (kbd "RET") 'vimp-ret)
(define-key vimp-motion-state-map "\\" 'vimp-execute-in-emacs-state)
(define-key vimp-motion-state-map "z^" 'vimp-scroll-top-line-to-bottom)
(define-key vimp-motion-state-map "z+" 'vimp-scroll-bottom-line-to-top)
(define-key vimp-motion-state-map "zt" 'vimp-scroll-line-to-top)
;; TODO: z RET has an advanced form taking an count before the RET
;; but this requires again a special state with a single command
;; bound to RET
(define-key vimp-motion-state-map (vconcat "z" [return]) "zt^")
(define-key vimp-motion-state-map (kbd "z RET") (vconcat "z" [return]))
(define-key vimp-motion-state-map "zz" 'vimp-scroll-line-to-center)
(define-key vimp-motion-state-map "z." "zz^")
(define-key vimp-motion-state-map "zb" 'vimp-scroll-line-to-bottom)
(define-key vimp-motion-state-map "z-" "zb^")
(define-key vimp-motion-state-map "v" 'vimp-visual-char)
(define-key vimp-motion-state-map "V" 'vimp-visual-line)
(define-key vimp-motion-state-map "\C-v" 'vimp-visual-block)
(define-key vimp-motion-state-map "gv" 'vimp-visual-restore)
(define-key vimp-motion-state-map (kbd "C-^") 'vimp-buffer)
(define-key vimp-motion-state-map [left] 'vimp-backward-char)
(define-key vimp-motion-state-map [right] 'vimp-forward-char)
(define-key vimp-motion-state-map [up] 'vimp-previous-line)
(define-key vimp-motion-state-map [down] 'vimp-next-line)
(define-key vimp-motion-state-map "zl" 'vimp-scroll-column-right)
(define-key vimp-motion-state-map [?z right] "zl")
(define-key vimp-motion-state-map "zh" 'vimp-scroll-column-left)
(define-key vimp-motion-state-map [?z left] "zh")
(define-key vimp-motion-state-map "zL" 'vimp-scroll-right)
(define-key vimp-motion-state-map "zH" 'vimp-scroll-left)
(define-key vimp-motion-state-map
  (read-kbd-macro vimp-toggle-key) 'vimp-emacs-state)

;; text objects
(define-key vimp-outer-text-objects-map "w" 'vimp-a-word)
(define-key vimp-outer-text-objects-map "W" 'vimp-a-WORD)
(define-key vimp-outer-text-objects-map "s" 'vimp-a-sentence)
(define-key vimp-outer-text-objects-map "p" 'vimp-a-paragraph)
(define-key vimp-outer-text-objects-map "b" 'vimp-a-paren)
(define-key vimp-outer-text-objects-map "(" 'vimp-a-paren)
(define-key vimp-outer-text-objects-map ")" 'vimp-a-paren)
(define-key vimp-outer-text-objects-map "[" 'vimp-a-bracket)
(define-key vimp-outer-text-objects-map "]" 'vimp-a-bracket)
(define-key vimp-outer-text-objects-map "B" 'vimp-a-curly)
(define-key vimp-outer-text-objects-map "{" 'vimp-a-curly)
(define-key vimp-outer-text-objects-map "}" 'vimp-a-curly)
(define-key vimp-outer-text-objects-map "<" 'vimp-an-angle)
(define-key vimp-outer-text-objects-map ">" 'vimp-an-angle)
(define-key vimp-outer-text-objects-map "'" 'vimp-a-single-quote)
(define-key vimp-outer-text-objects-map "\"" 'vimp-a-double-quote)
(define-key vimp-outer-text-objects-map "`" 'vimp-a-back-quote)
(define-key vimp-outer-text-objects-map "t" 'vimp-a-tag)
(define-key vimp-outer-text-objects-map "o" 'vimp-a-symbol)
(define-key vimp-inner-text-objects-map "w" 'vimp-inner-word)
(define-key vimp-inner-text-objects-map "W" 'vimp-inner-WORD)
(define-key vimp-inner-text-objects-map "s" 'vimp-inner-sentence)
(define-key vimp-inner-text-objects-map "p" 'vimp-inner-paragraph)
(define-key vimp-inner-text-objects-map "b" 'vimp-inner-paren)
(define-key vimp-inner-text-objects-map "(" 'vimp-inner-paren)
(define-key vimp-inner-text-objects-map ")" 'vimp-inner-paren)
(define-key vimp-inner-text-objects-map "[" 'vimp-inner-bracket)
(define-key vimp-inner-text-objects-map "]" 'vimp-inner-bracket)
(define-key vimp-inner-text-objects-map "B" 'vimp-inner-curly)
(define-key vimp-inner-text-objects-map "{" 'vimp-inner-curly)
(define-key vimp-inner-text-objects-map "}" 'vimp-inner-curly)
(define-key vimp-inner-text-objects-map "<" 'vimp-inner-angle)
(define-key vimp-inner-text-objects-map ">" 'vimp-inner-angle)
(define-key vimp-inner-text-objects-map "'" 'vimp-inner-single-quote)
(define-key vimp-inner-text-objects-map "\"" 'vimp-inner-double-quote)
(define-key vimp-inner-text-objects-map "`" 'vimp-inner-back-quote)
(define-key vimp-inner-text-objects-map "t" 'vimp-inner-tag)
(define-key vimp-inner-text-objects-map "o" 'vimp-inner-symbol)
(define-key vimp-motion-state-map "gn" 'vimp-next-match)
(define-key vimp-motion-state-map "gN" 'vimp-previous-match)

(when vimp-want-C-i-jump
  (define-key vimp-motion-state-map (kbd "C-i") 'vimp-jump-forward))

(when vimp-want-C-u-scroll
  (define-key vimp-motion-state-map (kbd "C-u") 'vimp-scroll-up))

;;; Visual state

(define-key vimp-visual-state-map "A" 'vimp-append)
(define-key vimp-visual-state-map "I" 'vimp-insert)
(define-key vimp-visual-state-map "o" 'exchange-point-and-mark)
(define-key vimp-visual-state-map "O" 'vimp-visual-exchange-corners)
(define-key vimp-visual-state-map "R" 'vimp-change)
(define-key vimp-visual-state-map "u" 'vimp-downcase)
(define-key vimp-visual-state-map "U" 'vimp-upcase)
(define-key vimp-visual-state-map "z=" 'ispell-word)
(define-key vimp-visual-state-map "a" vimp-outer-text-objects-map)
(define-key vimp-visual-state-map "i" vimp-inner-text-objects-map)
(define-key vimp-visual-state-map (kbd "<insert>") 'undefined)
(define-key vimp-visual-state-map (kbd "<insertchar>") 'undefined)
(define-key vimp-visual-state-map [remap vimp-repeat] 'undefined)
(define-key vimp-visual-state-map [escape] 'vimp-exit-visual-state)

;;; Operator-Pending state

(define-key vimp-operator-state-map "a" vimp-outer-text-objects-map)
(define-key vimp-operator-state-map "i" vimp-inner-text-objects-map)
;; (define-key vimp-operator-state-map [escape] 'keyboard-quit)

;;; Insert state

(defvar vimp-insert-state-bindings
  `(("\C-v" . quoted-insert)
    ("\C-k" . vimp-insert-digraph)
    ("\C-o" . vimp-execute-in-normal-state)
    ("\C-r" . vimp-paste-from-register)
    ("\C-y" . vimp-copy-from-above)
    ("\C-e" . vimp-copy-from-below)
    ("\C-n" . vimp-complete-next)
    ("\C-p" . vimp-complete-previous)
    ("\C-x\C-n" . vimp-complete-next-line)
    ("\C-x\C-p" . vimp-complete-previous-line)
    ("\C-t" . vimp-shift-right-line)
    ("\C-d" . vimp-shift-left-line)
    ("\C-a" . vimp-paste-last-insertion)
    ([remap delete-backward-char] . vimp-delete-backward-char-and-join)
    ,(if vimp-want-C-w-delete
         '("\C-w" . vimp-delete-backward-word)
       '("\C-w" . vimp-window-map))
    ([mouse-2] . mouse-yank-primary))
  "Evil's bindings for insert state (for
`vimp-insert-state-map'), excluding <delete>, <escape>, and
`vimp-toggle-key'.")

(defun vimp-update-insert-state-bindings (&optional _option-name remove force)
  "Update bindings in `vimp-insert-state-map'.
If no arguments are given add the bindings specified in
`vimp-insert-state-bindings'. If REMOVE is non nil, remove only
these bindings. Unless FORCE is non nil, this will not
overwriting existing bindings, which means bindings will not be
added if one already exists for a key and only default bindings
are removed.

Note that <delete>, <escape> and `vimp-toggle-key' are not
included in `vimp-insert-state-bindings' by default."
  (interactive)
  (dolist (binding vimp-insert-state-bindings)
    (cond
     ((and remove
           (or force
               ;; Only remove if the default binding has not changed
               (eq (lookup-key vimp-insert-state-map (car binding))
                   (cdr binding))))
      (define-key vimp-insert-state-map (car binding) nil))
     ((and (null remove)
           (or force
               ;; Check to see that nothing is bound here before adding
               (null (lookup-key vimp-insert-state-map (car binding)))))
      (define-key vimp-insert-state-map (car binding) (cdr binding))))))

(define-key vimp-insert-state-map [delete] 'delete-char)
(define-key vimp-insert-state-map [escape] 'vimp-normal-state)
(define-key vimp-insert-state-map
  (read-kbd-macro vimp-toggle-key) 'vimp-emacs-state)

;;; Replace state

(define-key vimp-replace-state-map (kbd "DEL") 'vimp-replace-backspace)
(define-key vimp-replace-state-map [escape] 'vimp-normal-state)

;;; Emacs state

(define-key vimp-emacs-state-map
  (read-kbd-macro vimp-toggle-key) 'vimp-exit-emacs-state)

(when vimp-want-C-w-in-emacs-state
  (define-key vimp-emacs-state-map "\C-w" 'vimp-window-map))

;;; Mouse
(define-key vimp-motion-state-map [down-mouse-1] 'vimp-mouse-drag-region)
(define-key vimp-visual-state-map [mouse-2] 'vimp-exit-visual-and-repeat)
(define-key vimp-normal-state-map [mouse-2] 'mouse-yank-primary)

;; Ex
(define-key vimp-motion-state-map ":" 'vimp-ex)
(define-key vimp-motion-state-map "!" 'vimp-shell-command)

(vimp-ex-define-cmd "e[dit]" 'vimp-edit)
(vimp-ex-define-cmd "w[rite]" 'vimp-write)
(vimp-ex-define-cmd "wa[ll]" 'vimp-write-all)
(vimp-ex-define-cmd "sav[eas]" 'vimp-save)
(vimp-ex-define-cmd "r[ead]" 'vimp-read)
(vimp-ex-define-cmd "b[uffer]" 'vimp-buffer)
(vimp-ex-define-cmd "bn[ext]" 'vimp-next-buffer)
(vimp-ex-define-cmd "bp[revious]" 'vimp-prev-buffer)
(vimp-ex-define-cmd "bN[ext]" "bprevious")
(vimp-ex-define-cmd "sb[uffer]" 'vimp-split-buffer)
(vimp-ex-define-cmd "sbn[ext]" 'vimp-split-next-buffer)
(vimp-ex-define-cmd "sbp[revious]" 'vimp-split-prev-buffer)
(vimp-ex-define-cmd "sbN[ext]" "sbprevious")
(vimp-ex-define-cmd "buffers" 'buffer-menu)
(vimp-ex-define-cmd "files" 'vimp-show-files)
(vimp-ex-define-cmd "ls" "buffers")

(vimp-ex-define-cmd "c[hange]" 'vimp-change)
(vimp-ex-define-cmd "co[py]" 'vimp-copy)
(vimp-ex-define-cmd "t" "copy")
(vimp-ex-define-cmd "m[ove]" 'vimp-move)
(vimp-ex-define-cmd "d[elete]" 'vimp-delete)
(vimp-ex-define-cmd "y[ank]" 'vimp-yank)
(vimp-ex-define-cmd "go[to]" 'vimp-goto-char)
(vimp-ex-define-cmd "j[oin]" 'vimp-join)
(vimp-ex-define-cmd "le[ft]" 'vimp-align-left)
(vimp-ex-define-cmd "ri[ght]" 'vimp-align-right)
(vimp-ex-define-cmd "ce[nter]" 'vimp-align-center)
(vimp-ex-define-cmd "sp[lit]" 'vimp-window-split)
(vimp-ex-define-cmd "vs[plit]" 'vimp-window-vsplit)
(vimp-ex-define-cmd "new" 'vimp-window-new)
(vimp-ex-define-cmd "vne[w]" 'vimp-window-vnew)
(vimp-ex-define-cmd "clo[se]" 'vimp-window-delete)
(vimp-ex-define-cmd "on[ly]" 'delete-other-windows)
(vimp-ex-define-cmd "q[uit]" 'vimp-quit)
(vimp-ex-define-cmd "wq" 'vimp-save-and-close)
(vimp-ex-define-cmd "quita[ll]" 'vimp-quit-all)
(vimp-ex-define-cmd "qa[ll]" "quitall")
(vimp-ex-define-cmd "wqa[ll]" 'vimp-save-and-quit)
(vimp-ex-define-cmd "xa[ll]" "wqall")
(vimp-ex-define-cmd "x[it]" 'vimp-save-modified-and-close)
(vimp-ex-define-cmd "exi[t]" 'vimp-save-modified-and-close)
(vimp-ex-define-cmd "bd[elete]" 'vimp-delete-buffer)
(vimp-ex-define-cmd "bw[ipeout]" 'vimp-delete-buffer)
(vimp-ex-define-cmd "g[lobal]" 'vimp-ex-global)
(vimp-ex-define-cmd "v[global]" 'vimp-ex-global-inverted)
(vimp-ex-define-cmd "norm[al]" 'vimp-ex-normal)
(vimp-ex-define-cmd "s[ubstitute]" 'vimp-ex-substitute)
(vimp-ex-define-cmd "&" 'vimp-ex-repeat-substitute)
(vimp-ex-define-cmd "&&" 'vimp-ex-repeat-substitute-with-flags)
(vimp-ex-define-cmd "~" 'vimp-ex-repeat-substitute-with-search)
(vimp-ex-define-cmd "~&" 'vimp-ex-repeat-substitute-with-search-and-flags)
(vimp-ex-define-cmd "registers" 'vimp-show-registers)
(vimp-ex-define-cmd "marks" 'vimp-show-marks)
(vimp-ex-define-cmd "ju[mps]" 'vimp-show-jumps)
(vimp-ex-define-cmd "noh[lsearch]" 'vimp-ex-nohighlight)
(vimp-ex-define-cmd "f[ile]" 'vimp-show-file-info)
(vimp-ex-define-cmd "<" 'vimp-shift-left)
(vimp-ex-define-cmd ">" 'vimp-shift-right)
(vimp-ex-define-cmd "=" 'vimp-ex-line-number)
(vimp-ex-define-cmd "!" 'vimp-shell-command)
(vimp-ex-define-cmd "@:" 'vimp-ex-repeat)
(vimp-ex-define-cmd "mak[e]" 'vimp-make)
(vimp-ex-define-cmd "cc" 'vimp-goto-error)
(vimp-ex-define-cmd "cfir[st]" 'first-error)
(vimp-ex-define-cmd "cr[ewind]" 'first-error)
(vimp-ex-define-cmd "cn[ext]" 'next-error)
(vimp-ex-define-cmd "cp[revious]" 'previous-error)
(vimp-ex-define-cmd "set-initial-state" 'vimp-ex-set-initial-state)
(vimp-ex-define-cmd "show-digraphs" 'vimp-ex-show-digraphs)
(vimp-ex-define-cmd "sor[t]" 'vimp-ex-sort)
(vimp-ex-define-cmd "res[ize]" 'vimp-ex-resize)

;; search command line
(define-key vimp-ex-search-keymap "\d" #'vimp-ex-delete-backward-char)
(define-key vimp-ex-search-keymap "\C-r" 'vimp-paste-from-register)
(define-key vimp-ex-search-keymap "\C-n" 'next-history-element)
(define-key vimp-ex-search-keymap "\C-p" 'previous-history-element)

;; ex command line
(define-key vimp-ex-completion-map "\d" #'vimp-ex-delete-backward-char)
(define-key vimp-ex-completion-map "\t" #'vimp-ex-completion)
(define-key vimp-ex-completion-map [tab] #'vimp-ex-completion)
(define-key vimp-ex-completion-map [remap completion-at-point] #'vimp-ex-completion)
(define-key vimp-ex-completion-map "\C-a" 'vimp-ex-completion)
(define-key vimp-ex-completion-map "\C-b" 'move-beginning-of-line)
(define-key vimp-ex-completion-map "\C-c" 'abort-recursive-edit)
(define-key vimp-ex-completion-map "\C-d" 'vimp-ex-completion)
(define-key vimp-ex-completion-map "\C-g" 'abort-recursive-edit)
(define-key vimp-ex-completion-map "\C-k" 'vimp-insert-digraph)
(define-key vimp-ex-completion-map "\C-l" 'vimp-ex-completion)
(define-key vimp-ex-completion-map "\C-p" #'previous-complete-history-element)
(define-key vimp-ex-completion-map "\C-r" 'vimp-paste-from-register)
(define-key vimp-ex-completion-map "\C-n" #'next-complete-history-element)
(define-key vimp-ex-completion-map "\C-u" 'vimp-delete-whole-line)
(define-key vimp-ex-completion-map "\C-v" #'quoted-insert)
(define-key vimp-ex-completion-map "\C-w" 'backward-kill-word)
(define-key vimp-ex-completion-map [escape] 'abort-recursive-edit)
(define-key vimp-ex-completion-map [S-left] 'backward-word)
(define-key vimp-ex-completion-map [S-right] 'forward-word)
(define-key vimp-ex-completion-map [up] 'previous-complete-history-element)
(define-key vimp-ex-completion-map [down] 'next-complete-history-element)
(define-key vimp-ex-completion-map [prior] 'previous-history-element)
(define-key vimp-ex-completion-map [next] 'next-history-element)
(define-key vimp-ex-completion-map [return] 'exit-minibuffer)
(define-key vimp-ex-completion-map (kbd "RET") 'exit-minibuffer)

;; vimp-read-key
(define-key vimp-read-key-map (kbd "ESC") #'keyboard-quit)
(define-key vimp-read-key-map (kbd "C-]") #'keyboard-quit)
(define-key vimp-read-key-map (kbd "C-q") #'vimp-read-quoted-char)
(define-key vimp-read-key-map (kbd "C-v") #'vimp-read-quoted-char)
(define-key vimp-read-key-map (kbd "C-k") #'vimp-read-digraph-char)
(define-key vimp-read-key-map "\r" "\n")

;; command line window
(vimp-define-key 'normal
  vimp-command-window-mode-map (kbd "RET") 'vimp-command-window-execute)
(vimp-define-key 'insert
  vimp-command-window-mode-map (kbd "RET") 'vimp-command-window-execute)

(provide 'vimp-maps)

;;; vimp-maps.el ends here
