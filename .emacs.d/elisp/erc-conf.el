;; ERC
;; ERC is an IRC client
(setq erc-header-line-format "%o" ;; just the channel title in title bar
;      erc-insert-timestamp-function 'erc-insert-timestamp-left ;; timestamp on the left
      erc-interpret-mirc-color t
      erc-modules '(autojoin ;; rejoin chans as soon as possible
                    button
                    completion ;; complete commands and nicks
                    fill
                    irccontrols
                    keep-place
                    list
                    log
                    match
                    menu
                    move-to-prompt
                    netsplit
                    networks
                    noncommands
                    readonly
                    ring
                    smiley
                    stamp
                    spelling
                    track)
      erc-nick "Harry"
      erc-nick-uniquifier "_"
;      erc-timestamp-format "<%T>" ;; HH:MM:SS
;      erc-timestamp-format-left "<%T>"
      erc-user-full-name "Raphaël Cauderlier"
      ;erc-warn-about-blank-lines nil
      erc-whowas-on-nosuchnick t ;; if nick is unknown, use whowas instead of whois
      erc-join-buffer 'window-noselect
      erc-track-switch-direction 'importance)

(add-hook 'erc-mode-hook (lambda () (global-hl-line-mode 1))) ;; highlight current line

(setq erc-autojoin-channels-alist
      '(("irc" "#A♡" "#info" "#crans" "#roots" "#emacs" "#ca" "#pimeys")
	("localhost" "#general")))
;(erc :server "irc")
;(erc :server "localhost")

(provide 'erc-conf)
