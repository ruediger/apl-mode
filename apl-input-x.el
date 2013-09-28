;;; apl-input-x --- APL extended input method -*- lexical-binding: t -*-

;; Copyright (C) 2013 Rüdiger Sonderfeld

;; Author: Rüdiger Sonderfeld <ruediger@c-plusplus.de>
;; Created: 28 Sep 2013
;; Version: 1.0
;; Keywords: input, APL
;; URL: https://github.com/ruediger/apl-mode

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'quail)

(quail-define-package
 "APL-X" "UTF-8" "⍝" t
 "This is an extended input method for APL.
It is based on \"APL-UNICODE\" but adds more convenient bindings for many
symbols.  E.g., <- for ← or // for ⍝."
 '(("\t" . quail-completion))
 t nil nil t nil nil nil nil nil t)

(eval-when-compile
  (require 'apl-input))

(eval
 `(quail-define-rules
   ,@(apl-input-generate-rules) ;; Add rules from apl-input.el
   (":=" ?←)
   ("<-" ?←)
   ("->" ?→)
   ("each" ?¨)
   ("&" ?∧)
   ("and" ?∧)
   ("nand" ?⍲)
   ("or" ?∨)
   ("nor" ?⍱)
   ("*" ?×)
   ("%" ?÷)
   ("/" ?÷)
   ("log" ?⍟)
   ("==" ?=)
   ("max" ?⌈)
   ("ceil" ?⌈)
   ("min" ?⌊)
   ("floor" ?⌊)
   ("in" ?∈)
   ("!=" ?≠)
   ("<=" ?≤)
   (">=" ?≥)
   ("^" ?⋆)
   ("rot" ?⌽)
   ("flip" ?⍉)
   ("iota" ?⍳)
   ("rho" ?⍴)
   ("take" ?↑)
   ("drop" ?↓)
   ("upg" ?⍋)
   ("dng" ?⍒)
   ("pack" ?⊥)
   ("unpack" ?⊤)
   ("bag" ?⊂)
   ("pick" ?⊃)
   ("eval" ?⍎)
   ("form" ?⍕)
   ("rtrack" ?⊢)
   ("where" ?⊣)
   ("pi" ?○)
   ;;("mdiv ?)
   ("rand" ??)
   ("beam" ?⌶)
   ("ref" ?%)
   ("dot" ?∪)
   ("nabla" ?∇)
   ("del" ?∇)
   ("sys" ?⎕)
   ("//" ?⍝)
   ("alpha" ?⍺)
   ("omega" ?⍵)
   ;; TODO ...
   ))

(provide 'apl-input-x)

;;; apl-input-x.el ends here
