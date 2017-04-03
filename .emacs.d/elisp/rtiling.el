;;; rtiling.el --- Dynamic tiling window management
;;
;; Filename: rtiling.el
;; Description: Dynamic tiling window management
;; Author: Raphaël Cauderlier
;; Maintainer: Raphaël Cauderlier
;; Copyright (C) 2014, Raphaël Cauderlier.
;; Version: 0.1
;;
;;; Commentary:
;;
;; This is not part of Emacs.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Code:


(defvar rtiling-orientation :horizontal)

(defun rtiling-display-full ()
  "Arrange the windows according to the :full orientation."
  (delete-other-windows))

(defun rtiling-buffer-ignore-p (b)
  "Test wether B should be ignored."
  (or (string-prefix-p " " (or (buffer-name b) " ")) ; Hidden buffers
      (string-prefix-p "*" (or (buffer-name b) "")) ; Application buffers
      (string-prefix-p ".newsrc" (or (buffer-name b) "")) ; Gnus internal buffer
      (not (buffer-live-p b))                             ; Killed buffers
      (eq which-key--buffer b)
      (minibufferp b)
      (persp-buffer-in-other-p b)
      )
  )

(defvar rtiling-buffer-listing-function #'buffer-list)

(defun rtinling-display-horizontal ()
  "Arrange the windows according to the :horizontal orientation."
  (let ((w (selected-window))
        (all-buffers (funcall rtiling-buffer-listing-function)))
    (setq all-buffers (remove (window-buffer w) all-buffers))
    ;; Remove all killed and hidden buffers
    (dolist (b all-buffers)
      (when (rtiling-buffer-ignore-p b)
        (setq all-buffers (remove b all-buffers))))
    (when all-buffers
      (delete-other-windows)
      (select-window (split-window-horizontally))
      (switch-to-buffer (car all-buffers))
      (setq all-buffers (cdr all-buffers))
      (dolist (b all-buffers)
        (select-window (split-window-vertically 3))
        (switch-to-buffer b)
      )
    (balance-windows)
    (other-window 1)
    )
  ))

(defun rtiling-display-vertical ()
  "Arrange the windows according to the :vertical orientation."
  (let ((w (selected-window))
        (all-buffers (funcall rtiling-buffer-listing-function)))
    (setq all-buffers (remove (window-buffer w) all-buffers))
    ;; Remove all killed and hidden buffers
    (dolist (b all-buffers)
      (when (rtiling-buffer-ignore-p b)
        (setq all-buffers (remove b all-buffers))))
    (when all-buffers
      (delete-other-windows)
      (select-window (split-window-vertically))
      (switch-to-buffer (car all-buffers))
      (setq all-buffers (cdr all-buffers))
      (dolist (b all-buffers)
        (select-window (split-window-horizontally 3))
        (switch-to-buffer b)
      )
      (balance-windows)
      (other-window 1)
    )
  ))

(defun rtiling-display-orientation ()
  "Redisplay the buffers according to `rtiling-orientation'."
  (cond
   ((eq rtiling-orientation :horizontal) (rtinling-display-horizontal))
   ((eq rtiling-orientation :vertical) (rtiling-display-vertical))
   (t (rtiling-display-full)))
  )

(defun rtiling-detect-orientation ()
  "Guess current orientation and assign it to `rtiling-orientation'."
  (let ((tree (car (window-tree))))
    (setq rtiling-orientation
          (cond
           ((one-window-p) :full)
           ((listp tree) (if (car tree) :vertical :horizontal))
           (t :full)))))

(defun rtiling-change-orientation ()
  "Cycle the value of the `rtiling-orientation' variable."
  (interactive)
  (rtiling-detect-orientation)
  (setq rtiling-orientation
        (cond
         ((eq rtiling-orientation :horizontal) :vertical)
         ((eq rtiling-orientation :vertical) :full)
         (t :horizontal)))
  (rtiling-display-orientation)
  )

;; Inspired by XMonad behaviour and Marcin Borkowski's other-window-or-switch-buffer
(defun rtiling-other-buffer-or-window ()
  "Equivalent to `other-window' in :horizontal and :vertical orientation but switch to `other-buffer' in :full orientation."
  (interactive)
  (rtiling-detect-orientation)
  (if (eq rtiling-orientation :full)
      (switch-to-buffer (other-buffer))
    (other-window 1))
  )

(defun rtiling-switch-windows ()
  "Exchange selected window and master window."
  (interactive)
  (let* ((master (frame-first-window))
         (current (selected-window))
         (mbuff (window-buffer master))
         (cbuff (window-buffer current)))
    (set-window-buffer current mbuff)
    (set-window-buffer master cbuff)
    (select-window master)
    )
  )

(defun rtiling-kill-current-window ()
  "Destroy the current window and remove the associated buffer from the current perspective."
  (interactive)
  (let ((w (selected-window))
        (b (current-buffer)))
    (delete-window w)
    (persp-remove-buffer b)
    (balance-windows)
    )
  )

(defun rtiling-find-file ()
  "Find a file in a new window."
  (interactive)
  (select-window (frame-first-window))
  ;; Previous window is -2 because -1 is minibuffer
  (other-window (- 2))
  (if (eq rtiling-orientation :vertical)
      (select-window (split-window-horizontally 3))
    (select-window (split-window-vertically 3))
    (setq rtiling-orientation :horizontal)
    )
  (balance-windows)
  (ido-find-file))

(provide 'rtiling)
;;; rtiling.el ends here
