;;; struct-completion.el --- Keyword slot completion for cl-defstruct constructors -*- lexical-binding: t -*-

;; Author: Gino Cornejo <gggion123@gmail.com>
;; Maintainer: Gino Cornejo <gggion123@gmail.com>
;; URL: https://github.com/gggion/struct-completion.el
;; Keywords: lisp, completion

;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Complete keyword slot arguments inside `cl-defstruct' constructor
;; calls with slot names, types, and documentation.
;;
;; Basic usage:
;;
;;     (add-hook 'emacs-lisp-mode-hook #'struct-completion-mode)
;;
;; When typing keyword arguments inside a constructor call like
;; (make-my-struct :sl|), the package offers slot-aware completion
;; with type annotations and per-slot documentation.
;;
;; All candidate data comes from runtime introspection via
;; `cl--class-slots' cross-referenced with constructor arglists
;; from `help-split-fundoc'.  The struct definition must be
;; evaluated in the current session for completion to work.
;;
;; The package provides:
;; - `struct-completion-mode': buffer-local minor mode
;; - `struct-completion-at-point': capf for keyword slot completion
;; - `struct-completion-detectors': extensible detector registry

;;; Code:

(require 'cl-lib)

;;;; Detector Registry

(defvar struct-completion-detectors nil
  "List of detector functions for keyword slot completion.
Each function receives HEAD-SYMBOL and returns a list of
\(KEYWORD-STRING . SLOT-PLIST) entries, or nil.  First non-nil
result wins.

SLOT-PLIST keys:

  `:type'           Type specifier or nil.
  `:documentation'  Documentation string or nil.
  `:default'        Default value expression.
  `:read-only'      Non-nil if slot is read-only.
  `:inherited-from' Ancestor type symbol, or nil for own slots.
  `:struct-type'    The struct type symbol.

Managed by `struct-completion-mode'.
Queried by `struct-completion--run-detectors'.")

;;;; Internal Variables

(defvar struct-completion--doc-buffer nil
  "Reusable buffer for slot documentation display.
Created on demand by function `struct-completion--doc-buffer'.
Populated by `struct-completion--render-doc'.")

;;;; Constructor Pattern

(defconst struct-completion--constructor-pattern
  "Constructor for objects of type[[:space:]\n]+`\\([^']+\\)'"
  "Regexp matching `cl-defstruct' constructor docstrings.
Capture group 1 holds the type name.  Handles both single-line
and newline-wrapped forms produced by `internal--format-docstring-line'.

Used by `struct-completion--detect-cl-struct'.")

;;;; Doc Buffer

(defun struct-completion--doc-buffer ()
  "Return the reusable documentation buffer, creating it if needed.

Also see variable `struct-completion--doc-buffer'."
  (or (and (buffer-live-p struct-completion--doc-buffer)
           struct-completion--doc-buffer)
      (setq struct-completion--doc-buffer
            (get-buffer-create " *struct-completion-doc*"))))

;;;; Head Symbol Detection

(defun struct-completion--head-symbol ()
  "Return the head symbol of the immediately enclosing form, or nil.
Navigate backward with `up-list', skip whitespace and comments,
then read the symbol name with `scan-sexps'.

Called by `struct-completion-at-point'."
  (save-excursion
    (condition-case nil
        (progn
          (up-list -1)
          (forward-char 1)
          (forward-comment (buffer-size))
          (let ((s (point))
                (e (ignore-errors (scan-sexps (point) 1))))
            (when e
              (intern-soft (buffer-substring-no-properties s e)))))
      (scan-error nil))))

;;;; Keyword Bounds Detection

(defun struct-completion--keyword-bounds ()
  "Return (BEG . END) for the keyword prefix at point, or nil.
A keyword prefix starts with a colon followed by zero or more
word or symbol-constituent characters.

Called by `struct-completion-at-point'."
  (let ((end (point))
        beg)
    (save-excursion
      (skip-syntax-backward "w_")
      (when (eq (char-before) ?:)
        (setq beg (1- (point))))
      ;; Handle bare ":" with nothing after it.
      (when (and (null beg) (eq (char-after (1- end)) ?:))
        (setq beg (1- end))))
    (when beg
      (cons beg end))))

;;;; Arglist Parsing

(defun struct-completion--parse-key-params (arglist-str)
  "Extract &key parameter names from ARGLIST-STR as downcased strings.
ARGLIST-STR is the arglist portion returned by `help-split-fundoc'.
Return nil if no &key section is present.

Called by `struct-completion--cl-struct-slots'."
  (when (string-match "&key" arglist-str)
    (let ((key-section (substring arglist-str (match-end 0))))
      (when (string-match ")\\'" key-section)
        (setq key-section (substring key-section 0 (match-beginning 0))))
      (cl-loop for token in (split-string key-section nil t)
               for clean = (string-trim token "(" ")")
               unless (or (string-empty-p clean)
                          (string-prefix-p "&" clean))
               collect (downcase clean)))))

;;;; Ancestor Introspection

(defun struct-completion--ancestor-slot-plist (type-sym slot-name)
  "Return the slot plist for SLOT-NAME from an ancestor of TYPE-SYM.
Walk the `:include' chain via `cl-find-class' and
`cl--struct-class-parents' until a slot descriptor with a
non-empty plist is found.  Return nil if no ancestor carries
metadata for SLOT-NAME.

This recovers `:type' and `:documentation' lost when an
`:include' form overrides a slot default.

Called by `struct-completion--cl-struct-slots'."
  (let ((class (cl-find-class type-sym)))
    (cl-loop for parent in (and class (cl--struct-class-parents class))
             for parent-name = (cl--struct-class-name parent)
             for parent-slot = (and parent-name
                                    (assq slot-name
                                          (condition-case nil
                                              (cl-struct-slot-info parent-name)
                                            (error nil))))
             for parent-plist = (cddr parent-slot)
             when parent-plist return parent-plist
             ;; Recurse into grandparents when the immediate parent
             ;; also lacks metadata for this slot.
             for ancestor-plist = (and parent-name
                                      (struct-completion--ancestor-slot-plist
                                       parent-name slot-name))
             when ancestor-plist return ancestor-plist)))

(cl-defun struct-completion--ancestor-slots (type-sym)
  "Return alist mapping inherited slot names to ancestor type symbols.
Walk the `:include' chain of TYPE-SYM via `cl-find-class' and
`cl--struct-class-parents' using a breadth-first work queue.
Each entry is (SLOT-NAME . ANCESTOR-TYPE-SYM).

Stop at `cl-structure-object' and skip non-struct parents
such as `built-in-class'.  Return nil if TYPE-SYM has no
`:include' ancestors.

Uses `cl--struct-class-parents' and `cl-struct-slot-info'
for ancestor introspection.
Called by `struct-completion--cl-struct-slots'."
  (let ((class (cl-find-class type-sym)))
    (unless class (cl-return-from struct-completion--ancestor-slots))
    (let ((queue (cl--struct-class-parents class))
          result)
      (while queue
        (let ((parent (pop queue)))
          (when (cl-typep parent 'cl-structure-class)
            (let ((parent-name (cl--struct-class-name parent)))
              (when (and parent-name
                         (not (eq parent-name 'cl-structure-object)))
                (dolist (slot (condition-case nil
                                  (cl-struct-slot-info parent-name)
                                (error nil)))
                  (let ((name (car slot)))
                    (when (and (symbolp name)
                               (not (memq name '(cl-tag-slot cl-skip-slot)))
                               (not (assq name result)))
                      (push (cons name parent-name) result))))
                ;; Enqueue grandparents for deeper chains.
                (let ((parent-class (cl-find-class parent-name)))
                  (when parent-class
                    (dolist (gp (cl--struct-class-parents parent-class))
                      (push gp queue)))))))))
      result)))

;;;; Slot Extraction

(cl-defun struct-completion--cl-struct-slots (type-sym arglist-str)
  "Return slot alist for TYPE-SYM filtered by &key params in ARGLIST-STR.
Each entry is (KEYWORD-STRING . SLOT-PLIST).

KEYWORD-STRING is the slot name prefixed with a colon.
SLOT-PLIST contains `:type', `:documentation', `:default',
`:read-only', `:inherited-from', and `:struct-type'.

Slot metadata comes from `cl--class-slots' via `cl-find-class'.
Only slots whose names appear as &key parameters in ARGLIST-STR
become candidates.

When an inherited slot lacks `:type' or `:documentation' due to
`:include' default overrides, recover them from ancestor types
via `struct-completion--ancestor-slot-plist'.

Called by `struct-completion--detect-cl-struct'."
  (let ((class (cl-find-class type-sym))
        (valid-names (struct-completion--parse-key-params arglist-str)))
    (unless (and class valid-names)
      (cl-return-from struct-completion--cl-struct-slots))
    (let ((slots (cl--class-slots class))
          (ancestor-map (struct-completion--ancestor-slots type-sym)))
      (cl-loop for slot across slots
               for name = (cl--slot-descriptor-name slot)
               for name-str = (when (symbolp name) (symbol-name name))
               ;; Cross-reference against arglist to exclude slots not exposed
               ;; as &key parameters (e.g., phantom slots injected at runtime
               ;; by third-party packages like elisp-def).
               when (and name-str (member name-str valid-names))
               collect
               (let* ((type (cl--slot-descriptor-type slot))
                      (type (unless (eq type t) type))
                      (doc (alist-get :documentation
                                      (cl--slot-descriptor-props slot)))
                      (ancestor (cdr (assq name ancestor-map))))
                 ;; Recover metadata lost by :include default overrides.
                 (when (and ancestor (not (or type doc)))
                   (when-let* ((ancestor-plist
                                (struct-completion--ancestor-slot-plist
                                 type-sym name)))
                     (unless type
                       (setq type (plist-get ancestor-plist :type)))
                     (unless doc
                       (setq doc (plist-get ancestor-plist :documentation)))))
                 (cons (format ":%s" name-str)
                       (list :type type
                             :documentation doc
                             :default (cl--slot-descriptor-initform slot)
                             :read-only (alist-get :read-only
                                                   (cl--slot-descriptor-props slot))
                             :inherited-from ancestor
                             :struct-type type-sym)))))))

;;;; cl-defstruct Detector

(cl-defun struct-completion--detect-cl-struct (head-sym)
  "Return slot alist if HEAD-SYM is a `cl-defstruct' &key constructor.
Return nil if HEAD-SYM is not a defined function, its docstring
does not match the constructor pattern, or the arglist lacks &key.

Registered in `struct-completion-detectors'.
Called by `struct-completion--run-detectors'."
  ;; Detection chain:
  ;; 1. `fboundp' guard
  ;; 2. `documentation' retrieval
  ;; 3. `struct-completion--constructor-pattern' match
  ;; 4. `help-split-fundoc' arglist extraction
  ;; 5. &key presence check (rejects BOA constructors)
  ;; 6. `struct-completion--cl-struct-slots' for candidate generation
  (unless (fboundp head-sym)
    (cl-return-from struct-completion--detect-cl-struct))
  (let ((doc (documentation head-sym t)))
    (unless (and doc
                 (string-match struct-completion--constructor-pattern doc))
      (cl-return-from struct-completion--detect-cl-struct))
    (let* ((type-name (match-string 1 doc))
           (split (help-split-fundoc doc head-sym))
           (arglist-str (car split)))
      (unless (and type-name split
                   (string-match-p "&key" arglist-str))
        (cl-return-from struct-completion--detect-cl-struct))
      (when-let* ((type-sym (intern-soft type-name)))
        (struct-completion--cl-struct-slots type-sym arglist-str)))))

;;;; Detector Dispatch

(defun struct-completion--run-detectors (head-sym)
  "Run detectors in `struct-completion-detectors' for HEAD-SYM.
Return the first non-nil result, or nil if no detector matches.

Called by `struct-completion-at-point'."
  (cl-loop for detector in struct-completion-detectors
           thereis (funcall detector head-sym)))

;;;; Annotation and Documentation Display

(defun struct-completion--format-type (plist)
  "Return an annotation string for a slot described by PLIST.
Display the `:type' specifier when present, truncating to 25
characters.  Fall back to \" slot\" when no type is declared.
Append \" ^\" when `:inherited-from' is non-nil.

Called by the `:annotation-function' in `struct-completion-at-point'."
  (let ((type (plist-get plist :type))
        (inherited (plist-get plist :inherited-from))
        base)
    (setq base (if type
                   (let ((s (format " %s" type)))
                     (if (> (length s) 25)
                         (concat (substring s 0 22) "...")
                       s))
                 " slot"))
    (if inherited
        (concat base " ^")
      base)))

(defun struct-completion--fontify-type (type)
  "Return a fontified string for type specifier TYPE.
Print TYPE into a temporary `emacs-lisp-mode' buffer and apply
`font-lock-ensure' to produce syntax-highlighted text.

Called by `struct-completion--render-doc'."
  (let ((str (format "%s" type)))
    (with-temp-buffer
      (insert str)
      (delay-mode-hooks (emacs-lisp-mode))
      (font-lock-ensure)
      (buffer-string))))

(defun struct-completion--fontify-doc (doc)
  "Return DOC with `symbol' references fontified.
Apply `font-lock-constant-face' to each `...' quotation,
matching the style of provenance lines in the doc buffer.

Called by `struct-completion--render-doc'."
  (with-temp-buffer
    (insert doc)
    (goto-char (point-min))
    (while (re-search-forward "`\\([^']+\\)'" nil t)
      (put-text-property (match-beginning 0) (match-end 0)
                         'face 'font-lock-constant-face))
    (buffer-string)))

(defun struct-completion--render-doc (cand plist)
  "Regnder slot documentation for CAND with metadata PLIST into the doc buffer.
PLIST is the slot metadata from a detector result entry.
Return the buffer, or nil if PLIST contains no displayable data.

Lines are omitted when their data is nil.  The separator between
metadata and documentation appears only when both sections exist.

Uses variable `struct-completion--doc-buffer' for the display buffer.
Uses `struct-completion--fontify-type' for type highlighting.
Called by the `:company-doc-buffer' in `struct-completion-at-point'."
  ;; Display layout:
  ;;   CAND (bold)
  ;;   Slot of `struct-type'.
  ;;   Inherited from `ancestor-type'.
  ;;   Type: fontified-specifier
  ;;   Read-only: yes
  ;;   Default: value
  ;;
  ;;   Documentation text.
  (let ((type        (plist-get plist :type))
        (doc         (plist-get plist :documentation))
        (default     (plist-get plist :default))
        (read-only   (plist-get plist :read-only))
        (inherited   (plist-get plist :inherited-from))
        (struct-type (plist-get plist :struct-type)))
    (when (or type doc read-only inherited struct-type)
      (with-current-buffer (struct-completion--doc-buffer)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize cand 'face 'bold) "\n\n")
          (when struct-type
            (insert "Slot of "
                    (propertize (format "`%s'" struct-type)
                                'face 'font-lock-constant-face)
                    ".\n"))
          (when inherited
            (insert "Inherited from "
                    (propertize (format "`%s'" inherited)
                                'face 'font-lock-constant-face)
                    ".\n"))
          (when type
            (insert "Type: "
                    (struct-completion--fontify-type type)
                    "\n"))
          (when read-only
            (insert "Read-only: yes\n"))
          (when default
            (insert (format "Default: %S\n" default)))
          (when (and (or type default read-only inherited struct-type) doc)
            (insert "\n"))
          (when doc
            (insert (struct-completion--fontify-doc doc) "\n")))
        (current-buffer)))))

;;;; Capf Core

(cl-defun struct-completion-at-point ()
  "Complete keyword slot arguments in struct constructor calls.
Return nil when not at a keyword position inside a recognized
constructor, otherwise return (BEG END TABLE . PLIST).

Guard against activation inside strings and comments via
`syntax-ppss'.  Detect context with `struct-completion--head-symbol'
and `struct-completion--run-detectors'.

Added to `completion-at-point-functions' by `struct-completion-mode'.
Uses `struct-completion--keyword-bounds' for region detection
and `struct-completion--run-detectors' for candidate generation."
  ;; Guard: reject inside string or comment.
  (let ((ppss (syntax-ppss)))
    (when (or (nth 3 ppss) (nth 4 ppss))
      (cl-return-from struct-completion-at-point)))

  (when-let* ((bounds (struct-completion--keyword-bounds))
              (beg (car bounds))
              (end (cdr bounds))
              (head-sym (struct-completion--head-symbol))
              (slot-alist (struct-completion--run-detectors head-sym)))
    ;; Captured state: slot-alist is shared by all closures below.
    (let ((candidates (mapcar #'car slot-alist))
          (kind-fn (lambda (_) 'field))
          (ann-fn
           (lambda (cand)
             (when-let* ((entry (assoc cand slot-alist)))
               (struct-completion--format-type (cdr entry)))))
          (docsig-fn
           (lambda (cand)
             (when-let* ((entry (assoc cand slot-alist))
                         (doc (plist-get (cdr entry) :documentation)))
               (car (split-string doc "\n")))))
          (doc-fn
           (lambda (cand)
             (when-let* ((entry (assoc cand slot-alist)))
               (struct-completion--render-doc cand (cdr entry))))))

      (list beg end candidates
            :exclusive 'no
            :company-kind kind-fn
            :annotation-function ann-fn
            :company-docsig docsig-fn
            :company-doc-buffer doc-fn))))

;;;; Minor Mode

;;;###autoload
(define-minor-mode struct-completion-mode
  "Complete struct slot keywords in constructor calls.
When enabled, add `struct-completion-at-point' to the buffer-local
`completion-at-point-functions' at depth -10 so it runs before
`elisp-completion-at-point'.

Also see `struct-completion-detectors'."
  :lighter nil
  :group 'lisp
  (if struct-completion-mode
      (add-hook 'completion-at-point-functions
                #'struct-completion-at-point -10 'local)
    (remove-hook 'completion-at-point-functions
                 #'struct-completion-at-point 'local)))

;;;; Detector Registration

;; Register the built-in detector
(add-to-list 'struct-completion-detectors
             #'struct-completion--detect-cl-struct)

(provide 'struct-completion)
;;; struct-completion.el ends here
