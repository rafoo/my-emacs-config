;; Configuration of Windows and panes look

(set-scroll-bar-mode 'right)
(add-hook
 'after-change-major-mode-hook
 (lambda () (setq indicate-buffer-boundaries 'right)) )

;; fullscreen toogle by F11
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn
                             (setq old-fullscreen current-value)
                             'fullboth) ) ) ) )
(global-set-key [f11] 'toggle-fullscreen)
  
;; Make new frames fullscreen by default. Note: this hook doesn't do
;; anything to the initial frame if it's in your .emacs, since that file is
;; read _after_ the initial frame is created.
(add-hook 'after-make-frame-functions 'toggle-fullscreen)

(provide 'windows-conf)