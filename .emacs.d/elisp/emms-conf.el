;; EMMS
;; EMMS is a media player, to start a playlist use M-x emms-play-playlist-directory-tree
;; from EMMS info pages
(when (require 'emms nil t)
  (emms-mode-line-enable)
  (emms-mode-line-restore-mode-line)
  (emms-playing-time-enable-display)
;  (defadvice emms-mode-line (around header activate) (apply-header (lambda nil ad-do-it)))
;  (defadvice emms-mode-line-restore-mode-line (around header activate) (apply-header (lambda nil ad-do-it)))
;  (defadvice emms-playing-time-mode-line (around header activate) (apply-header (lambda nil ad-do-it)))
  (require 'emms-setup)
  (apply-header 'emms-all)
  (emms-default-players)
  (setq emms-source-file-default-directory "~/Musique/")
  (setq emms-mode-line-titlebar-function
	(lambda ()
	  (emms-track-description (emms-playlist-current-selected-track))))
  )

(provide 'emms-conf)
