
;; Give password to OfflineImap
;; Source: http://www.emacswiki.org/emacs/OfflineIMAP#toc2
(defun offlineimap-get-password (host port)
  "Give password to OfflineImap"
  (message "OfflineImap Get Password")
  (let ((result (car (auth-source-search :host host :port port))))
    (funcall (plist-get result :secret))))

;; Start Emacs Server to ensure that OfflineIMAP will find a client
(require 'server)
(unless (server-running-p) (server-start))

(provide 'offlineimap-conf)
