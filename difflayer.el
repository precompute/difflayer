;;; difflayer --- Overlay diffs on selected regions -*- lexical-binding: t; -*-

;; Copyright (C) 2025 precompute

;; Author: precompute <git@precompute.net>
;; URL: https://github.com/precompute/legit
;; Created: May 26, 2025
;; Modified: September 17, 2026
;; Version: 0.5.1
;; Package-Requires: ((emacs "26.1"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Difflayer takes two regions and overlays their diff on them.

;;; Code:
;;;; Variables
(defcustom difflayer-highlight-selections t
  "When non-nil, highlight selected regions."
  :type 'boolean :group 'difflayer)

(defcustom difflayer-diff-style 'git-word
  "Diff style to use.
diff     : GNU Diff
git      : git diff (line)
git-word : git diff (word)
git-char : git diff (char)"
  :type 'symbol :group 'difflayer
  :options (list 'diff 'git 'git-char 'git-word))

(defcustom difflayer-deactivate-mark t
  "When non-nil, mark is deactivated after a region is selected."
  :type 'boolean :group 'difflayer)

(defcustom difflayer-preserve-first-selection nil
  "When non-nil, selections are FILO."
  :type 'boolean :group 'difflayer)

(defcustom difflayer-do-not-check-for-binaries nil
  "When non-nil, difflayer will not check for git / diff binaries."
  :type 'boolean :group 'difflayer)

;;;; Internal
(defvar difflayer--current-diff nil
  "Stores the current diff output.")

(defvar difflayer--metadata nil
  "Stores the overlays, region bounds and buffers of the two selected regions.
Keys: :bounds :overlay :buffer")

(defvar difflayer--commands
  (list 'ediff ""
        'diff (list "diff" "-s" "-U0")
        'git (list "git" "--no-pager" "diff" "--no-index" "-U0")
        'git-word (list "git" "--no-pager" "diff" "--no-index" "-U0" "--word-diff=porcelain" "--word-diff-regex=[^[:space:]]+|[[:space:]]+")
        'git-char (list "git" "--no-pager" "diff" "--no-index" "-U0" "--word-diff=porcelain" "--word-diff-regex=."))
  "Commands associated with `difflayer-diff-style’.")

(defvar difflayer--exit-code nil
  "Exit code for the previous process call by Difflayer.")

(defvar difflayer--done nil
  "States whether difflayer has diffed two regions.")

;;;; Faces
(defface difflayer-highlight-face
  '((t :inherit highlight))
  "Face for highlighting selected regions."
  :group 'difflayer)

(defface difflayer-diff-added
  '((t :inherit diff-added))
  "Face for highlighting new parts of text."
  :group 'difflayer)

(defface difflayer-diff-removed
  '((t :inherit diff-removed))
  "Face for highlighting deleted parts of text."
  :group 'difflayer)

(defface difflayer-rule-face-1
  '((t :underline t :extend t))
  "Face for horizontal rule before selection regions."
  :group 'difflayer)

(defface difflayer-rule-face
  '((t :overline t :extend t))
  "Face for horizontal rule after selection regions."
  :group 'difflayer)

;;;; Functions
;;;;; Check for binaries
(defun difflayer--check-for-binaries ()
  "Check whether the diff and git binaries exist on this system."
  (unless (and (executable-find "diff")
               (executable-find "git"))
    (error "Difflayer couldn't find the 'git' and/or 'diff' binaries.")))

;;;;; Register
(defun difflayer--register-region (&optional metadata)
  "Register the currently active region.
Use METADATA (:bounds :buffer) if specified."
  (when (> (length difflayer--metadata) 1)
    (difflayer--remove-selection-overlay (if difflayer-preserve-first-selection
                                             (car difflayer--metadata)
                                           (cadr difflayer--metadata)))
    (setq difflayer--metadata (if difflayer-preserve-first-selection
                                  (cdr difflayer--metadata)
                                (list (car difflayer--metadata)))))
  (let ((metadata (if metadata
                      metadata
                    (list :bounds (car (region-bounds)) :buffer (current-buffer)))))
    (push (if difflayer-highlight-selections
              (difflayer--add-selection-overlay metadata)
            metadata)
          difflayer--metadata))
  (when difflayer-deactivate-mark (deactivate-mark)))

;;;;; Clear
(defun difflayer--clear-vars (&optional everything)
  "Clear all registered regions and stored diffs.
When EVERYTHING is non-nil, clear all vars and overlays."
  (setq difflayer--metadata nil)
  (setq difflayer--exit-code nil)
  (when everything
    (setq difflayer--current-diff nil)
    (setq difflayer--done nil)
    (difflayer--remove-all-selection-overlays)))

;;;;; Message
(defun difflayer--message (add del)
  "Displays a message with number of lines changed (ADD and DEL)."
  (message "Diff: %s insertion%s, %s deletion%s"
           add (if (> add 1) "s" "") del (if (> del 1) "s" "")))

;;;;; Overlay
;;;;;; Selection
;;;;;;; Add
(defun difflayer--add-selection-overlay (metadata)
  "Add overlay with face `difflayer-highlight-face’ to region in METADATA."
  (with-current-buffer (plist-get metadata :buffer)
    (let ((overlay (make-overlay (car (plist-get metadata :bounds))
                                 (cdr (plist-get metadata :bounds)))))
      (overlay-put overlay 'difflayer-highlight t)
      (overlay-put overlay 'face 'difflayer-highlight-face)
      (overlay-put overlay 'before-string (propertize " " 'display '(space :align-to right) 'face 'difflayer-rule-face-1))
      (overlay-put overlay 'after-string (propertize " " 'display '(space :align-to right) 'face 'difflayer-rule-face))
      (plist-put metadata :overlay overlay))))

(defun difflayer--add-all-selection-overlays ()
  "Add all selection overlays."
  (unless (= 0 (length difflayer--metadata))
    (mapc #'difflayer--add-selection-overlay difflayer--metadata)))

;;;;;;; Remove
(defun difflayer--remove-selection-overlay (metadata)
  "Remove overlay in METADATA."
  (let ((buf (plist-get metadata :buffer)))
    (if (buffer-live-p buf)
        (with-current-buffer buf
          (delete-overlay (plist-get metadata :overlay)))
      (difflayer--clear-vars t)))) ;; If a registered buffer no longer exists, clear everything.

(defun difflayer--remove-all-selection-overlays ()
  "Remove all selection overlays."
  (if (= 0 (length difflayer--metadata))
      (mapc (lambda (b)
              (when (buffer-live-p b)
                (with-current-buffer b
                  (mapc (lambda (o)
                          (when (overlay-get o 'difflayer-highlight)
                            (delete-overlay o)))
                        (car (overlay-lists))))))
            (buffer-list))
    (mapc #'difflayer--remove-selection-overlay difflayer--metadata)))

;;;;;; Diff
;;;;;;; Add Negative Overlay
(defun difflayer--add-negative-overlay (beg end)
  "Add an overlay that shows removal of content between BEG and END."
  (let ((overlay (make-overlay beg end)))
    (overlay-put overlay 'difflayer-highlight t)
    (overlay-put overlay 'face 'difflayer-diff-removed)))

;;;;;;; Add Positive Overlay
(defun difflayer--add-positive-overlay (beg end)
  "Add an overlay that shows addition of content between BEG and END."
  (let ((overlay (make-overlay beg end)))
    (overlay-put overlay 'difflayer-highlight t)
    (overlay-put overlay 'face 'difflayer-diff-added)))

;;;;; Get Diff
(defun difflayer--get-diff ()
  "Call the command associated with `difflayer-diff-style’."
  (let ((tempf0 (make-temp-file "difflayer0"))
        (tempf1 (make-temp-file "difflayer1"))
        (result))
    (setq difflayer--metadata (reverse difflayer--metadata))
    (unwind-protect
        (let* ((r0 (plist-get (car difflayer--metadata) :bounds))
               (r1 (plist-get (cadr difflayer--metadata) :bounds))
               (exec (car (plist-get difflayer--commands difflayer-diff-style)))
               (args (append (cdr (plist-get difflayer--commands difflayer-diff-style))
                             (list "--" tempf0 tempf1)))
               (inhibit-message t))
          (with-current-buffer (plist-get (car difflayer--metadata) :buffer)
            (write-region (car r0) (cdr r0) tempf0))
          (with-current-buffer (plist-get (cadr difflayer--metadata) :buffer)
            (write-region (car r1) (cdr r1) tempf1))
          (setq result
                (apply #'process-lines-handling-status exec #'difflayer--status-handler args)))
      (mapc #'delete-file (list tempf0 tempf1))
      result)))

;;;;;; Status Handler
(defun difflayer--status-handler (status)
  "Handles STATUS for `process-lines-handling-status’ in `difflayer--diff’."
  (setq difflayer--exit-code status)
  t)

;;;;; Overlay Diff
(defun difflayer--overlay-diff (d)
  "Overlay the diff D over regions in `difflayer--metadata’."
  (setq difflayer--current-diff d) ;; debug
  (pcase difflayer-diff-style
    ('git (mapc #'difflayer--overlay-diff--git d))
    ('diff (mapc #'difflayer--overlay-diff--diff d))
    ('git-word (difflayer--overlay-diff--git-word (nthcdr 4 d)))
    ('git-char (difflayer--overlay-diff--git-char (nthcdr 4 d)))
    (_ nil))
  (setq difflayer--done t))

;;;;;; diff-type: git
(defun difflayer--overlay-diff--git (line)
  "Make overlays for a LINE from a git-style diff D.
This function doesn’t consider parse anything other than hunk info,
which looks like the following:
@@ -N[,N] +N[,N] @@"
  (when (= ?@ (aref line 0))
    (let* ((buf1 (plist-get (car difflayer--metadata) :buffer))
           (buf2 (plist-get (cadr difflayer--metadata) :buffer))
           (l1 (with-current-buffer buf1
                 (line-number-at-pos (car (plist-get (car difflayer--metadata) :bounds)))))
           (l2 (with-current-buffer buf2
                 (line-number-at-pos (car (plist-get (cadr difflayer--metadata) :bounds)))))
           (indices ;; returns ((a b) (c d))
            (mapcar (lambda (e) (mapcar #'string-to-number (split-string (substring e 1) ",")))
                    (butlast (cdr (split-string line)))))
           (a (+ l1 -1 (caar indices)))
           (b (cadar indices))
           (ins (if b b 1))
           (b (if b (if (= 0 b) b (+ a -1 b)) a))
           (c (+ l2 -1 (caadr indices)))
           (d (cadadr indices))
           (del (if d d 1))
           (d (if d (if (= 0 d) d (+ c -1 d)) c))
           (line-beg-pos (lambda (l) (save-excursion (goto-char (point-min))
                                                     (forward-line (- l 1))
                                                     (line-beginning-position))))
           (line-end-pos (lambda (l) (save-excursion (goto-char (point-min))
                                                     (forward-line (- l 1))
                                                     (line-end-position)))))
      (when (/= 0 b)
        (with-current-buffer buf1
          (difflayer--add-negative-overlay (funcall line-beg-pos a) (funcall line-end-pos b))))
      (when (/= 0 d)
        (with-current-buffer buf2
          (difflayer--add-positive-overlay (funcall line-beg-pos c) (funcall line-end-pos d))))
      (difflayer--message ins del))))

;;;;;; diff-type: diff
(defun difflayer--overlay-diff--diff (line)
  "Make overlays for a LINE from a diff-style diff D."
  (difflayer--overlay-diff--git line))

;;;;;; diff-type: git-char
(defun difflayer--overlay-diff--git-char (d)
  "Make overlays for git-char diff D."
  (let* ((buf1 (plist-get (car difflayer--metadata) :buffer))
         (buf2 (plist-get (cadr difflayer--metadata) :buffer))
         (p1 (car (plist-get (car difflayer--metadata) :bounds)))
         (p2 (car (plist-get (cadr difflayer--metadata) :bounds)))
         (line-beg-pos (lambda (l) (save-excursion (goto-char (point-min))
                                                   (forward-line (- l 1))
                                                   (line-beginning-position))))
         (has-negative-p (lambda (l) (catch 'has-negative
                                       (dolist (i l)
                                         (when (< i 0) (throw 'has-negative t))
                                         nil))))
         (calc-pos (lambda (firstpos linenum)
                     (let ((l (line-number-at-pos firstpos)))
                       (if (= 0 linenum)
                           (max firstpos (funcall line-beg-pos l))
                         (funcall line-beg-pos (+ linenum l))))))
         (process-poslist (lambda (posl total neg?)
                            (when (funcall has-negative-p posl)
                              (setq posl (reverse posl))
                              (if (> 0 (car posl)) (push 0 posl))
                              (let ((l) (acc 0) (total total))
                                (dolist (n posl)
                                  (if (> 0 n)
                                      (progn (push (+ acc total) l)
                                             (push (- (car l) n) l)
                                             (setq total (car l))
                                             (setq acc 0))
                                    (setq acc (+ acc n))))
                                (setq l (reverse l))
                                (while (< 1 (length l))
                                  (if neg?
                                      (difflayer--add-negative-overlay (car l) (cadr l))
                                    (difflayer--add-positive-overlay (car l) (cadr l)))
                                  (setq l (cddr l)))))))
         (dline 0) ;; current line number
         (neglist) ;; list of negative changes
         (poslist));; list of positive changes
    (mapc (lambda (line)
            (pcase (aref line 0)
              (?@ (setq dline
                        (1- (string-to-number
                             (car (split-string (substring (cadr (split-string line)) 1)
                                                ","))))))
              (?+ (push (- (1- (length line))) poslist))
              (?- (push (- (1- (length line))) neglist))
              (?~ (progn
                    (with-current-buffer buf1
                      (funcall process-poslist neglist (funcall calc-pos p1 dline) t))
                    (with-current-buffer buf2
                      (funcall process-poslist poslist (funcall calc-pos p2 dline) nil))
                    (setq neglist nil poslist nil dline (1+ dline))))
              (_ ((lambda (x) (push (1- x) poslist) (push (1- x) neglist))
                  (length line)))))
          d)))

;;;;;; diff-type: git-word
(defun difflayer--overlay-diff--git-word (d)
  "Make overlays for git-word diff D."
  (difflayer--overlay-diff--git-char d))

;;;;; Difflayer
(defun difflayer (arg)
  "Select two regions and overlay their diffs on them.
Select a region and call this function.  Repeat for the second region.
Call when region is inactive to diff the two previously selected regions.
Without a prefix ARG, use the diff style set in `difflayer-diff-style'.
With \\[universal-argument], allow dynamically choosing diff style.
With \\[universal-argument] \\[universal-argument], clear all state set by difflayer."
  (interactive "P")
  (let ((difflayer-diff-style difflayer-diff-style))
    (cond ((= 4 (prefix-numeric-value arg))
           (setq difflayer-diff-style
                 (intern (completing-read
                          "Diff Style:"
                          (caddr (custom-variable-type 'difflayer-diff-style))
                          nil t))))
          ((= 16 (prefix-numeric-value arg))
           (difflayer--clear-vars t)))
    (if difflayer--done (difflayer--clear-vars t))
    (if (region-active-p)
        (difflayer--register-region)
      (if (/= 2 (length difflayer--metadata))
          (message "Difflayer needs 2 regions to work.  You have registered %s."
                   (length difflayer--metadata))
        (unless difflayer-do-not-check-for-binaries (difflayer--check-for-binaries))
        (difflayer--remove-all-selection-overlays)
        (let ((d (difflayer--get-diff)))
          (pcase difflayer--exit-code
            (0 (message "Diff (%s): Regions are identical." difflayer-diff-style))
            (1 (difflayer--overlay-diff d))
            (_ (message "Difflayer: %s exit: %s" difflayer-diff-style difflayer--exit-code))))))))

(defun difflayer-reset ()
  "Reset Difflayer overlays and state.
Alternative: \\[universal-argument] \\[universal-argument] `difflayer'."
  (interactive)
  (difflayer--clear-vars t))

;;; provide
(provide 'difflayer)

;;; TODO (maybe)
;;;; Add a difflayer-reset function if the "arg" isn’t good enough
;;;; Use pcase-let* in difflayer--overlay-diff--git
;;; difflayer.el ends here
