;;; laptop --- Configuration that only makes sense on a laptop computer
;;; Commentary:

;;; Code:

;;; Battery
(display-battery-mode)

;; EMMS keys: these keys don't exist on a desktop keybord
(global-set-key (kbd "<XF86AudioPlay>") 'emms-pause)
(global-set-key (kbd "<XF86AudioStop>") 'emms-random)
(global-set-key (kbd "<XF86AudioPrev>") 'emms-previous)
(global-set-key (kbd "<XF86AudioNext>") 'emms-next)

(provide 'laptop)
;;; laptop.el ends here
