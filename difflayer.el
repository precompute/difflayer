;;; difflayer --- Overlay diffs on selected regions -*- lexical-binding: t; -*-

;; Copyright (C) 2025 precompute

;; Author: precompute <git@precompute.net>
;; URL: https://github.com/precompute/legit
;; Created: May 26, 2025
;; Modified: May 26, 2025
;; Version: 0.0.1
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

;;;; Internal
(defvar difflayer--current-diff nil
  "Stores the current diff output.")

(defvar difflayer--region-bounds nil
  "Stores the region bounds and buffers of the two selected regions.")

;;;; Faces
(defface difflayer-highlight-face
  '((t :inherit 'highlight))
  "Face for highlighting selected regions.")

;;;; Functions
;;;;; Register
(defun difflayer-register-region (&optional bounds)
  "Register the currently active region.
If BOUNDS are specified, use them instead of the current region."
  (when (> (length difflayer--region-bounds) 1)
    (difflayer--remove-selection-overlay (car difflayer--region-bounds))
    (setq difflayer--region-bounds (cdr difflayer--region-bounds)))
  (let ((bounds (cons (current-buffer) (if bounds bounds (region-bounds)))))
    (push bounds difflayer--region-bounds)
    (when difflayer-highlight-selections (difflayer--add-selection-overlay bounds))))

;;;;; Overlay
;;;;;; Add
(defun difflayer--add-selection-overlay (bounds)
  "Add overlay with face `difflayer-highlight-face’ to region in BOUNDS."
  (with-current-buffer (car bounds)
    (let* ((overlay (make-overlay (caadr bounds) (cdadr bounds))))
      (overlay-put overlay 'difflayer-highlight t)
      (overlay-put overlay 'face 'difflayer-highlight-face))))

;;;;;; Remove
(defun difflayer--remove-all-selection-overlays ()
  "Remove all selection overlays."
  (unless (= 0 (length difflayer--region-bounds))
    (dolist (bounds difflayer--region-bounds) (difflayer--remove-selection-overlay bounds))))

(defun difflayer--remove-selection-overlay (bounds)
  "Remove overlays from BOUNDS."
  (with-current-buffer (car bounds)
    ))
;;; provide
(provide 'difflayer)

;;; difflayer.el ends here
