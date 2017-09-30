;;; exwm-conf --- Configuration of EXWM

;;; Commentary:

;;; Code:

(require 'exwm-config)

(defun exwm-run (command)
  "Run a COMMAND in EXWM."
  (interactive (list (read-shell-command "$ ")))
  (start-process-shell-command command nil command))

(exwm-input-set-key
 (kbd "s-&")
 (lambda () (interactive) (exwm-workspace-switch 0)))
(exwm-input-set-key
 (kbd "s-é")
 (lambda () (interactive) (exwm-workspace-switch 1)))
(exwm-input-set-key
 (kbd "s-\"")
 (lambda () (interactive) (exwm-workspace-switch 2)))
(exwm-input-set-key
 (kbd "s-'")
 (lambda () (interactive) (exwm-workspace-switch 3)))

(exwm-input-set-key (kbd "s-r") #'exwm-reset)

(exwm-input-set-key (kbd "s-!") #'exwm-run)

(eval-after-load 'persp-conf
  '(progn
     (exwm-input-set-key (kbd "s-s") #'my-persp-switch)
     (exwm-input--update-global-prefix-keys)))

(eval-after-load 'rtiling
  '(progn
     (exwm-input-set-key (kbd "<s-tab>") #'rtiling-other-buffer-or-window)
     (exwm-input-set-key (kbd "s-SPC") #'rtiling-change-orientation)
     (exwm-input-set-key (kbd "<s-return>") #'rtiling-switch-windows)
     (exwm-input--update-global-prefix-keys)))

(exwm-input--update-global-prefix-keys)

;; Make class name the buffer name
(add-hook 'exwm-update-class-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name)))

(exwm-config-ido)

(exwm-enable)

(provide 'exwm-conf)
;;; exwm-conf.el ends here
