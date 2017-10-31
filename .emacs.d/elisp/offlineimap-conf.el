
;; Give password to OfflineImap
;; Source: http://www.emacswiki.org/emacs/OfflineIMAP#toc2
(defun offlineimap-get-password (host port)
  "Give password to OfflineImap"
  (message "OfflineImap Get Password")
  (let ((result (car (auth-source-search :host host :port port))))
    (funcall (plist-get result :secret))))

;; Start Emacs Server to ensure that OfflineIMAP will find a client
(server-start)

;; Run offlineimap from time to time
(run-with-idle-timer 60 'repeat 'offlineimap)

(provide 'offlineimap-conf)
