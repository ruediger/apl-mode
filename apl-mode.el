;;; apl-mode --- A mode for the APL programming language. -*- lexical-binding: t -*-

;; Copyright (C) 2013 Rüdiger Sonderfeld

;; Author: Rüdiger Sonderfeld <ruediger@c-plusplus.de>
;; Created: 27 Sep 2013
;; Version: 1.0
;; Keywords: apl, languages
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

(defgroup apl-mode nil
  "A mode for the APL programming language."
  :link '(url-link "https://github.com/ruediger/apl-mode")
  :prefix "apl-mode-"
  :group 'languages)

(defconst apl-comment-start "⍝"
  "Symbol to start a comment in APL.")

(defconst apl-characters (eval-when-compile
                           (let ((names (ucs-names)) chars)
                             (dolist (name names chars)
                               (when (string-match-p "^APL " (car name))
                                 (push name chars)))))
  "All APL Unicode characters.")

(declare-function inferior-apl-eval-region "inferior-apl" '(begin end))
(declare-function inferior-apl-eval-buffer "inferior-apl")
(declare-function inferior-apl-eval-line "inferior-apl" '(&optional arg))

(defvar apl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-hd" 'apl-help)
    (define-key map "\C-c\C-r" 'inferior-apl-eval-region) ;; TODO right key bindings?
    (define-key map "\C-c\C-b" 'inferior-apl-eval-buffer)
    (define-key map "\C-c\C-l" 'inferior-apl-eval-buffer)
    map)
  "Keymap for `apl-mode'.")

(defvar apl-functions
  '((?? ("Roll" . "One integer selected randomly from the first B integers")
        ("Deal" . "A distinct integer selected randomly from the first B integers"))
    (?⌈ ("Ceiling" . "Least integer greater than or equal to B")
        ("Maximum" . "The greater value of A or B"))
    (?⌊ ("Floor" . "Greatest integer less than or equal to B")
        ("Minimum" . "The smaller value of A or B"))
    (?⍴ ("Shape" . "Number of components in each dimension of B")
        ("Reshape" . "Array of shape A with data B"))
    (?∼ ("Not" . "Logical: ∼1 is 0, ∼0 is 1") nil)
    (?∣ ("Magnitude" . "Magnitude of B")
        ("Residue" . "B modulo A"))
    (?⍳ ("Index generator" . "Vector of the first B integers")
        ("Index of" . "The location (index) of B in A; 1+⌈/⍳⍴A if not found"))
    (?⋆ ("Exponential" . "e to the B power")
        ("Exponentiation" . "A raised to the B power"))
    (?− ("Negation" . "Changes sign of B")
        ("Subtract" . "A minus B"))
    (?+ ("Identity" . "No change to B")
        ("Add" . "Sum of A and B"))
    (?× ("Signum" . "¯1 if B<0; 0 if B=0; 1 if B>0")
        ("Multiply" . "A multiplied by B"))
    (?÷ ("Reciprocal" . "1 divided by B")
        ("Divide" . "A divided by B"))
    (?, ("Ravel" . "Reshape B into a vector")
        ("Catenation" . "Elements of B appended to the elements of A"))
    (?⌹ ("Matrix inverse". "Inverse of Matrix B")
        ("Matrix divide" . "Solution to system of linear equations Ax = B"))
    (?○ ("Pi times" . "Multiply by π")
        ("Circle" . "Trig functions of B selected by A. A=1: sin(B), 2: cos(B), 3: tan(B)"))
    (?⍟ ("Logarithm" . "Natural logarithm of B")
        ("Logarithm" . "Logarithm of B to base A"))
    (?⌽ ("Reversal" . "Reverse elements of B along last axis")
        ("Rotation" . "The elements of B are rotated A positions"))
    (?⊖ ("Reversal" . "Reverse elements of B along first axis")
        ("Rotation" . "The elements of B are rotated A positions along the first axis"))
    (?⍋ ("Grade up" . "Indices of B which will arrange B in ascending order") nil)
    (?⍒ ("Grade down" . "Indices of B which will arrange B in descending order") nil)
    (?⍎ ("Execute" . "Execute an APL expression") nil)
    (?⍕ ("Monadic format" . "A character representation of B")
        ("Dyadic format" . "Format B into a character matrix according to A"))
    (?⍉ ("Monadic transpose" . "Reverse the axes of B")
        ("General transpose" . "The axes of B are ordered by A"))
    (?! ("Factorial" . "Product of integers 1 to B")
        ("Combinations" . "Number of combinations of B taken A at a time"))
    (?∈ nil ("Membership" . "1 for elements of A present in B; 0 where not"))
    (?↑ nil ("Take" . "Select the first (or last) A elements of B according to ×A"))
    (?↓ nil ("Drop" . "Remove the first (or last) A elements of B according to ×A"))
    (?⊥ nil ("Decode" . "Value of polynomials whose coefficients are B at A"))
    (?⊤ nil ("Encode" . "Base-A representation of the value of B"))
    (?\\ nil ("Expansion" . "Insert zeros (or blanks) in B"))
    (?/ nil ("Compression" . "Select elements in B corresponding to ones in A"))
    (?< nil ("Less than" . "Comparison: 1 if true, 0 if false"))
    (?≤ nil ("Less than or equal" . "Comparison: 1 if true, 0 if false"))
    (?= nil ("Equal" . "Comparison: 1 if true, 0 if false"))
    (?≥ nil ("Greater than or equal" . "Comparison: 1 if true, 0 if false"))
    (?> nil ("Greater than" . "Comparison: 1 if true, 0 if false"))
    (?≠ nil ("Not equal" . "Comparison: 1 if true, 0 if false"))
    (?∨ nil ("Or" . "Logic: 0 if A and B are 0; 1 otherwise"))
    (?∧ nil ("And" . "Logic: 0 if A and B are 1; 1 otherwise"))
    (?⍱ nil ("Nor" . "Logic: 1 if both A and B are 0; 0 otherwise"))
    (?⍲ nil ("Nand" . "Logic: 0 if both A and B are 1; 1 otherwise"))
    ;; ...
    )
  "List of APL functions.
Copied from URL `https://en.wikipedia.org/wiki/APL_syntax_and_symbols'.
Format (CHARACTER MONADIC DYADIC):
  CHARACTER is the character representing the function.
  MONADIC and DYADIC are the description of the corresponding use.  Formated as
  (NAME . DESCRIPTION) or nil if not available.")

(defvar apl--function-regexp
  (concat "\\("
          (mapconcat #'(lambda (fun)
                         (regexp-quote (format "%c" (car fun))))
                     apl-functions "\\|")
          "\\)")
  "Regexp to find functions.")

(defvar apl-operators
  '((?/ . "Reduce (last axis)")
    (?⌿ . "Reduce (first axis)")
    (?\\ . "Scan (last axis)")
    (?⍀ . "Scan (first axis)")
    (?. . "Inner product")
    (?∘ . "Outer product (actually ∘.)")) ;; TODO
  "List of APL operators.
Copied from URL `https://en.wikipedia.org/wiki/APL_syntax_and_symbols'.
Format (CHARACTER . NAME).")

(defvar apl--operator-regexp
  (concat "\\(" (mapconcat #'(lambda (fun)
                               (regexp-quote (format "%c" (car fun))))
                           apl-operators "\\|")
          "\\)")
  "Regexp to find operators.")

(define-abbrev-table 'apl-mode-abbrev-table nil
  "Abbrev table for APL.")

(defvar apl-mode-syntax-table
  (let ((table (make-syntax-table)))
    (dolist (fun apl-functions)
      (modify-syntax-entry (car fun) "." table))
    (dolist (fun apl-operators)
      (modify-syntax-entry (car fun) "." table))
    ; (modify-syntax-entry ?¯ "." table)
    (modify-syntax-entry ?← "." table)
    (modify-syntax-entry ?⍝ "<" table)
    (modify-syntax-entry ?# "<" table) ;; Extension!
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?⍺ "_" table) ;; TODO "." instead of "_"?  Not a part of a symbol
    (modify-syntax-entry ?⍵ "_" table) ;;      But a symbol itself...
    (modify-syntax-entry ?⎕ "_" table)
    (modify-syntax-entry '(?a . ?z) "w" table)
    (modify-syntax-entry '(?A . ?Z) "w" table)
    (modify-syntax-entry ?' "\"" table)
    (dolist (paren '((?\( . ?\)) (?\[ . ?\]) (?\{ . ?\})))
      (modify-syntax-entry (car paren) (format "(%c" (cdr paren)) table)
      (modify-syntax-entry (cdr paren) (format ")%c" (car paren)) table))
    table)
  "Syntax table for `apl-mode'.")

(defvar apl-mode-font-lock-keywords ;; TODO does this even make sense?
  `(("\\(\\sw\\|\\s_\\)+" . 'font-lock-variable-name-face)
    (,(concat "\\(" (mapconcat #'(lambda (fun)
                                   (regexp-quote (format "%c" (car fun))))
                               apl-operators "\\|") "\\)")
     (1 'font-lock-keyword-face)))
  "Font lock keywords for `apl-mode'.")

;;;###autoload
(define-derived-mode apl-mode prog-mode "APL"
  "Major mode for editing APL code.

\\{apl-mode-map}"
  :abbrev-table apl-mode-abbrev-table
  :syntax-table apl-mode-syntax-table
  :group 'apl-mode

  (setq-local comment-start apl-comment-start)
  (setq-local comment-end "")
  (setq font-lock-defaults '(apl-mode-font-lock-keywords))
  (setq-local eldoc-documentation-function #'apl-eldoc-function))

(defun apl-eldoc-function ()
  "Add support for `eldoc-mode'."
  (cond
   ((looking-at apl--function-regexp)
    (let* ((symbol (match-string 1))
           (data (assq (aref symbol 0) apl-functions)))
      (save-excursion
        (skip-syntax-backward "-")
        (message "Data %s" data)
        (if (or (= (point) 1)
                (progn
                  (forward-char -1)
                  (or (looking-at "\n")
                      (looking-at apl--function-regexp)
                      (looking-at "\\s)"))))
            
            (concat "Monadic " symbol
                    (when data
                      (concat  " ["
                               (car (cadr data))
                               "]: "
                               (cdr (cadr data)))))
          (concat "Dyadic " symbol
                  (when data
                    (concat " ["
                            (car (nth 2 data))
                            "]: "
                            (cdr (nth 2 data)))))))))
   ((looking-at apl--operator-regexp)
    (let* ((symbol (match-string 1))
           (data (assq symbol apl-operators)))
      (concat "Operator " symbol
              (when data
                (concat " ["
                        (cdr data)
                        "]")))))))

(defcustom apl-help-buffer "*APL Help*"
  "Buffer name for `apl-help'."
  :type 'string
  :group 'apl-mode)

(define-derived-mode apl-help-mode help-mode "APLHelp"
  "Major mode for displaying APL documentation."
  :abbrev-table nil
  :syntax-table apl-mode-syntax-table
;  (setq-local info-lookup-mode 'apl-mode)
  )

(defun apl-help-completing-read ()
  "Read symbol name."
  (let (def)
    (when (or (looking-at apl--function-regexp)
              (looking-at apl--operator-regexp))
      (setq def (match-string 1)))
    (completing-read
     (format (if def "Symbol (default %s): "
               "Symbol: ") def)
     nil ;; TODO
     nil nil nil nil def 'inherit-input-method)))

(defun apl-help (symbol)
  "Provide help for SYMBOL."
  (interactive (list (apl-help-completing-read)))
  (with-current-buffer apl-help-buffer
    (apl-help-mode)
    (with-help-window apl-help-buffer
      (let ((help-xref-following t))
        (help-setup-xref (list #'apl-help symbol)
                         (called-interactively-p 'interactive)))
      ;; TODO system functions etc.
      (let* ((func (assq (aref symbol 0) apl-functions))
             (op (assq (aref symbol 0) apl-operators)))
        (cond
         (func
          (princ (format "%s is an APL function.\n\n" symbol))
          (when (cadr func)
            (princ (format "Monadic: %s\nUsage: %sB\n %s\n\n" (car (cadr func))
                           symbol
                           (cdr (cadr func)))))
          (when (car (cddr func))
            (princ (format "Dyadic: %s\nUsage: A%sB\n %s\n"
                           (car (car (cddr func)))
                           symbol
                           (cdr (car (cddr func)))))))
         (op
          (princ (format "%s is an APL operator: %s" symbol (cdr op))))
         (t
          (print (format "%s is unknown" symbol))))))))

;; TODO put this in an apl-mode-init function?
(add-to-list 'interpreter-mode-alist '("apl" . apl-mode))
(add-to-list 'auto-mode-alist '("\\.apl\\'" . apl-mode))

(provide 'apl-mode)

;;; apl-mode.el ends here
