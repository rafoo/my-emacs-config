;;; alsamixer-conf --- Wrapper around the alsamixer and amixer commands
;;; Commentary:

;;; Code:

;; Volume control using amixer
(defun amixer (arg)
  "Call amixer to set Master of the first card by ARG."
  (start-process "amixer" nil
                 "amixer" "sset" "Master" arg))

(defun vol- ()
  "Decrease volume using amixer."
  (interactive)
  (amixer "2%-"))

(defun vol+ ()
  "Increase volume using amixer."
  (interactive)
  (amixer "2%+"))

(defun vol0 ()
  "Toggle the mute state of the Master audio."
  (interactive)
  (start-process "amixer" nil
                 "amixer" "-D" "pulse" "set" "Master" "1+" "toggle"))

(global-set-key (kbd "<XF86AudioLowerVolume>") 'vol-)
(global-set-key (kbd "<XF86AudioRaiseVolume>") 'vol+)
(global-set-key (kbd "<pause>") 'vol0)

(eval-after-load 'exwm-conf
  '(defun alsamixer ()
     "Run alsamixer in a X terminal."
     (interactive)
     (exwm-run "xterm alsamixer")))

(global-set-key (kbd "C-c m") 'alsamixer)

(provide 'alsamixer-conf)
;;; alsamixer-conf.el ends here
