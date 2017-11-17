;;; desktop-conf --- Desktop configuration

;;; Commentary:
;; Save session when closing Emacs and reload it when restarting.

;;; Code:
(require 'desktop)
(desktop-save-mode 1)
(add-to-list 'desktop-globals-to-save 'command-history)
(add-to-list 'desktop-globals-to-save 'shell-command-history)
(add-to-list 'desktop-globals-to-save 'compile-history)

(setq desktop-files-not-to-save "\\(^/[^/:]*:\\|(ftp)$\\)\\|\\.\\(gpg\\|v\\)$"
      desktop-save t)

(provide 'desktop-conf)
;;; desktop-conf ends here
