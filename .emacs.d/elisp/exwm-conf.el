;;; exwm-conf --- Configuration of EXWM

;;; Commentary:

;;; Code:

(require 'exwm-config)

(declare-function rtiling-other-buffer-or-window "rtiling")
(declare-function rtiling-change-orientation "rtiling")
(declare-function rtiling-switch-windows "rtiling")
(declare-function rtiling-kill-current-window "rtiling")
(declare-function rtiling-find-file "rtiling")
(declare-function my-persp-switch "persp-conf")

(defun exwm-run (command)
  "Run a COMMAND in EXWM."
  (interactive (list (read-shell-command "$ ")))
  (start-process-shell-command command nil command))

(exwm-input-set-key (kbd "s-r") #'exwm-reset)
(exwm-input-set-key (kbd "s-!") #'exwm-run)

(eval-after-load 'persp-conf
  '(progn
     (exwm-input-set-key (kbd "s-s") #'my-persp-switch)
     (exwm-input--update-global-prefix-keys)))

(exwm-input-set-key (kbd "<s-tab>") #'rtiling-other-buffer-or-window)
(exwm-input-set-key (kbd "s-SPC") #'rtiling-change-orientation)
(exwm-input-set-key (kbd "<s-return>") #'rtiling-switch-windows)
(exwm-input-set-key (kbd "s-c") #'rtiling-kill-current-window)
(exwm-input-set-key (kbd "s-f") #'rtiling-find-file)
(exwm-input--update-global-prefix-keys)

(setq exwm-input-simulation-keys
 `((,(kbd "C-b") . left)
   (,(kbd "M-b") . C-left)
   (,(kbd "C-f") . right)
   (,(kbd "M-f") . C-right)
   (,(kbd "C-p") . up)
   (,(kbd "C-n") . down)
   (,(kbd "C-a") . home)
   (,(kbd "C-e") . end)
   (,(kbd "M-v") . prior)
   (,(kbd "C-v") . next)
   (,(kbd "C-d") . delete)
   (,(kbd "C-k") . (S-end delete))
   (,(kbd "C-w") . ?\C-x)
   (,(kbd "C-S-w") . ?\C-w) ;; To close Firefox tabs
   (,(kbd "M-w") . ?\C-c)
   (,(kbd "C-y") . ?\C-v)
   (,(kbd "C-_") . ?\C-z)
   (,(kbd "C-s") . ?\C-f)
   (,(kbd "C-o") . (return home left))
   ))

(add-hook 'exwm-manage-finish-hook
          (lambda ()
            (when (and exwm-class-name
                       (or (string= exwm-class-name "URxvt")
                           (string= exwm-class-name "Tilix")))
              (exwm-input-set-local-simulation-keys nil))))

(defvar last-file-name "")

(defun evince (filename)
  "Run evince and rename the buffer to FILENAME's basename."
  (interactive "f")
  (setq last-file-name (concat (file-name-base filename) ".pdf"))
  (start-process-shell-command "evince" nil (format "evince %s" filename))
  )

(add-hook 'exwm-manage-finish-hook
          (lambda ()
            (when (and exwm-class-name
                       (string= exwm-class-name "Evince"))
              (exwm-workspace-rename-buffer last-file-name))))

(exwm-input--update-global-prefix-keys)

;; Make class name the buffer name
(add-hook 'exwm-update-class-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name)))

(exwm-config-ido)

(exwm-enable)

(provide 'exwm-conf)
;;; exwm-conf.el ends here
