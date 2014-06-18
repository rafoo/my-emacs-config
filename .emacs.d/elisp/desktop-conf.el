;; Desktop configuration: save session when closing emacs and reload it when restarting

(desktop-save-mode 1)
(add-to-list 'desktop-globals-to-save 'command-history)
(add-to-list 'desktop-globals-to-save 'shell-command-history)
(add-to-list 'desktop-globals-to-save 'compile-history)

(setq desktop-files-not-to-save "\\(^/[^/:]*:\\|(ftp)$\\)\\|\\.\\(gpg\\|v\\)$"
      desktop-save t)

(provide 'desktop-conf)
