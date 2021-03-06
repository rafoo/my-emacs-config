;;; windows-conf --- Configuration of windows and panes look

;;; Commentary:
;;; Code:

(when (fboundp 'set-scroll-bar-mode) (set-scroll-bar-mode 'right))
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(add-hook
 'after-change-major-mode-hook
 (lambda () (setq indicate-buffer-boundaries 'right)) )

;; fullscreen toogle by F11
(defvar old-fullscreen nil)

(defun toggle-fullscreen ()
  "Toggle between displaying Emacs fullscreen or in an X window."
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn
                             (setq old-fullscreen current-value)
                             'fullboth) ) ) ) )
(global-set-key [f11] 'toggle-fullscreen)

(provide 'windows-conf)
;;; windows-conf ends here
