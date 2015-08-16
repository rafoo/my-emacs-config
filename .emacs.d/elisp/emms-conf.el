;; EMMS
;; EMMS is a media player, to start a playlist use M-x emms-play-playlist-directory-tree
;; from EMMS info pages

(require 'emms-mode-line)
(emms-mode-line-enable)
(emms-mode-line-restore-mode-line)

(require 'emms-playing-time)
(emms-playing-time-enable-display)

(require 'emms-setup)
(apply-header 'emms-all)
(emms-default-players)
(setq emms-source-file-default-directory "~/Musique/")
(setq emms-mode-line-titlebar-function
      (lambda ()
	(emms-track-description (emms-playlist-current-selected-track))))
(require 'emms-lyrics)
(emms-lyrics 1)
(setq emms-lyrics-display-on-minibuffer t)

(provide 'emms-conf)
