;; Desktop configuration: save session when closing emacs and reload it when restarting

(desktop-save-mode 1)
(add-to-list 'desktop-globals-to-save 'command-history)
(add-to-list 'desktop-globals-to-save 'shell-command-history)

(provide 'desktop-conf)
