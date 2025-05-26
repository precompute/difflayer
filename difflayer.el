;;; difflayer --- Overlay diffs on selected regions -*- lexical-binding: t; -*-

;; Copyright (C) 2025 precompute

;; Author: precompute <git@precompute.net>
;; URL: https://github.com/precompute/legit
;; Created: May 26, 2025
;; Modified: May 26, 2025
;; Version: 0.0.2
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

(defcustom difflayer-diff-style 'git
  "Diff style to use.
'ediff    : Emacs' built-in ediff
'diff     : GNU Diff
'git      : git diff
'git-word : git diff with word-level diff"
  :type 'symbol :group 'difflayer
  :options (list 'ediff 'diff 'git 'git-word))

(defcustom difflayer-deactivate-mark t
  "When non-nil, mark is deactivated after a region is selected."
  :type 'boolean :group 'difflayer)

(defcustom difflayer-preserve-first-selection nil
  "When non-nil, selecting more than one region overwrites the most recent selection."
  :type 'boolean :group 'difflayer)

;;;; Internal
(defvar difflayer--current-diff nil
  "Stores the current diff output.")

(defvar difflayer--metadata nil
  "Stores the overlays, region bounds and buffers of the two selected regions.
Keys: :bounds :overlay :buffer")

(defvar difflayer--commands
  (list 'ediff ""
        'diff (list "diff" "-u ")
        'git (list "git" "--no-pager" "diff" "--no-index")
        'git-word (list "git" "--no-pager" "diff" "--no-index" "--word"))
  "Commands associated with `difflayer-diff-style’.")

;;;; Faces
(defface difflayer-highlight-face
  '((t :inherit 'highlight))
  "Face for highlighting selected regions.")

;;;; Functions
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
  (when everything
    (setq difflayer--current-diff nil)
    (difflayer--remove-all-selection-overlays)))

;;;;; Overlay
;;;;;; Add
(defun difflayer--add-selection-overlay (metadata)
  "Add overlay with face `difflayer-highlight-face’ to region in METADATA."
  (with-current-buffer (plist-get metadata :buffer)
    (let* ((overlay (make-overlay (car (plist-get metadata :bounds))
                                  (cdr (plist-get metadata :bounds)))))
      (overlay-put overlay 'difflayer-highlight t)
      (overlay-put overlay 'face 'difflayer-highlight-face)
      (plist-put metadata :overlay overlay))))

(defun difflayer--add-all-selection-overlays ()
  "Add all selection overlays."
  (unless (= 0 (length difflayer--metadata))
    (mapc #'difflayer--add-selection-overlay difflayer--metadata)))

;;;;;; Remove
(defun difflayer--remove-selection-overlay (metadata)
  "Remove overlay in METADATA."
  (let ((buf (plist-get metadata :buffer)))
    (if (buffer-live-p buf)
        (with-current-buffer buf
          (delete-overlay (plist-get metadata :overlay)))
      (difflayer--clear-vars t))))

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

;;;;; Diff
(defun difflayer--diff ()
  "Diff the two regions in `difflayer--metadata’ and overlay the resulting diff over them."
  (let ((tempf0 (make-temp-file "difflayer0"))
        (tempf1 (make-temp-file "difflayer1")))
    (unwind-protect
        (let ((r0 (plist-get (car difflayer--metadata) :bounds))
              (r1 (plist-get (cadr difflayer--metadata) :bounds))
              (b0 (plist-get (car difflayer--metadata) :buffer))
              (b1 (plist-get (cadr difflayer--metadata) :buffer))
              (exec (car (plist-get difflayer--commands difflayer-diff-style)))
              (args (append (cdr (plist-get difflayer--commands difflayer-diff-style))
                            (list "--" tempf0 tempf1)))
              (inhibit-message t))
          (with-current-buffer b0 (write-region (car r0) (cdr r0) tempf0))
          (with-current-buffer b1 (write-region (car r1) (cdr r1) tempf1))
          (setq difflayer--current-diff
                (apply #'process-lines-handling-status exec #'difflayer--status-handler args)))
      (mapc #'delete-file (list tempf0 tempf1)))))

;;;;;; Status Handler
(defun difflayer--status-handler (status)
  "Handles STATUS for `process-lines-handling-status’ in `difflayer--diff’."
  (pcase status
    (0 (if (or (eq difflayer-diff-style 'git)
               (eq difflayer-diff-style 'git-word))
           (message "Selected regions are identical.")
         t))
    (1 (if (or (eq difflayer-diff-style 'git)
               (eq difflayer-diff-style 'git-word))
           t nil))))

;;;;; Difflayer
(defun difflayer ()
  "Select two regions and overlay their diffs on them.
Select a region and call this function.  Repeat for the second region.
Call when region is inactive to diff the two previously selected regions."
  (interactive)
  (if (region-active-p)
      (difflayer--register-region)
    (if (/= 2 (length difflayer--metadata))
        (message "Difflayer needs 2 regions to work.  You have registered %s."
                 (length difflayer--metadata))
      (difflayer--remove-all-selection-overlays)
      (difflayer--diff)
      (difflayer--clear-vars))))

;;; provide
(provide 'difflayer)

;;; difflayer.el ends here
