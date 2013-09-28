;;; apl-input --- APL Input Method -*- lexical-binding: t -*-

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
 "APL-UNICODE" "UTF-8" "⍝" t
 "An input method for Unicode APL characters.
The character names are the names of the Unicode symbols except for the
prefix \"APL FUNCTIONAL SYMBOL\".  Spaces are replaced by \"-\" and the names
are prefixed with $.

Examples
$iota → ⍳ (APL FUNCTIONAL SYMBOL IOTA)
$greater-than-diaeresis → ⍩ (APL FUNCTIONAL SYMBOL GREATER-THAN DIAERESIS)"
 '(("\t" . quail-completion))
 t t nil nil nil nil nil nil nil t)

;; TODO APL characters not in the APL FUNCTIONAL SYMBOL * set

(defun apl-input-generate-rules (&optional prefix)
  "Generate rules from `ucs-names'."
  (let ((prefix (or prefix "$"))
        (names (ucs-names)) rules)
    (dolist (name names rules)
      (when (string-match "^APL FUNCTIONAL SYMBOL \\(.*\\)" (car name))
        (push (list (concat prefix
                            (replace-regexp-in-string
                             " " "-"
                             (downcase (match-string 1 (car name)))))
                    (cdr name)) rules)))))

(eval
 `(quail-define-rules
   ,@(apl-input-generate-rules)
   ("$$" ?$)))

(provide 'apl-input)

;;; apl-input.el ends here
