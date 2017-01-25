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
  '(exwm-input-set-key (kbd "s-s") #'persp-switch)
  )

;; Make class name the buffer name
(add-hook 'exwm-update-class-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name)))

(exwm-config-ido)

(exwm-enable)

(provide 'exwm-conf)
;;; exwm-conf.el ends here
