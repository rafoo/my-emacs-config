;; Général

;; Mode novice
(setq gnus-novice-user t)

;; Topic

;; ;; les nouveaux groupes vont dans le topic qui les réclamme
;; (setq gnus-subscribe-newsgroup-method 'gnus-subscribe-topics)
;; ;; passe en topic-mode dès le lancement
;; (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

; (setq gnus-fetch-old-headers 'some) ;; Reconstruction des threads
(setq gnus-summary-gather-subject-limit 'fuzzy)

;; Vérification des nouveaux messages toutes les minutes
;; (require 'gnus-demon)
;; (gnus-demon-add-handler 'gnus-demon-scan-news 1 t)

;; Composition

;; Vérification orthographique
(add-hook 'message-setup-hook 'flyspell-mode)

;; Rééquilibre les paragraphes automatiquement
; (add-hook 'message-setup-hook 'refill-mode)
; Commenté parce que ça marche pas avec les headers et les signatures.

;; Sauvegarde tous les mails envoyés
(setq gnus-message-archive-group "Gnus-Sent")

;; Configuration des serveurs

;; Mails Cr@ns
;; Borrowed from http://sachachua.com/blog/2008/05/geek-how-to-use-offlineimap-and-the-dovecot-mail-server-to-read-your-gmail-in-emacs-efficiently/
(setq gnus-select-method '(nnimap "IMAP" (nnimap-address "localhost")
                                                     (nnimap-stream network)
                                                     (nnimap-authenticator login))
                                      )

;; ;; News
;; (setq gnus-secondary-select-methods '(
;;                                       (nntp "news.crans.org")
;; 				      (nntp "news.gmane.org")
;;                                       (nnmaildir "Cnam"
;;                                               (directory "~/OfflineIMAP/Cnam"))
;;                                       )
;;                                       )

;; ;; RSS
;; (setq nnrss-use-local t)


;; Envoi
(setq smtpmail-default-smtp-server "smtp.crans.org")
(setq smtpmail-smtp-server "smtp.crans.org")
(setq send-mail-function 'smtpmail-send-it)
(setq message-send-mail-function 'smtpmail-send-it)
(setq smtpmail-local-domain "crans.org")
(setq smtpmail-sendto-domain "crans.org")
(setq smtpmail-smtp-service 587)

(require 'smtpmail)

;; Display
(gnus-add-configuration
 '(article
   (horizontal 1.0
	       (vertical 50
			 (group 1.0))
	       (vertical 1.0
			 (summary 0.25 point)
			 (article 1.0)))))

(gnus-add-configuration
 '(summary
   (horizontal 1.0
	       (vertical 50
			 (group 1.0))
	       (vertical 1.0
			 (summary 1.0 point)))))

(setq-default gnus-summary-line-format "%U%R%z %(%&user-date; %* %B%a  %s%)\n"
	      gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
	      gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
	      gnus-thread-sort-functions '(gnus-thread-sort-by-date)
	      gnus-sum-thread-tree-false-root ""
	      gnus-sum-thread-tree-indent " "
	      gnus-sum-thread-tree-leaf-with-other "|-> "
	      gnus-sum-thread-tree-root ""
	      gnus-sum-thread-tree-single-leaf "L> "
	      gnus-sum-thread-tree-vertical "|")

;; HTML

;(setq mm-text-html-renderer 'w3m)
;(setq mm-discouraged-alternatives '("text/html" "text/richtext"))

;; Set the default value of mm-discouraged-alternatives.
(eval-after-load "gnus-sum"
  '(add-to-list
    'gnus-newsgroup-variables
    '(mm-discouraged-alternatives
      . '("text/html" "image/.*"))))

;; Display ‘text/html’ parts in nnrss groups.
(add-to-list
 'gnus-parameters
 '("\\`nnrss:" (mm-discouraged-alternatives nil)))

;; Attachments
(setq mm-enable-external 'ask)

;; nov
(setq nnimap-nov-is-evil t)

;; GPG
;; From https://www.emacswiki.org/emacs/EasyPG#to9copen

(require 'epg-config)
(setq mml2015-use 'epg

      mml2015-verbose t
      ; epg-user-id 1B199227E5ABC502
      mml2015-encrypt-to-self t
      mml2015-always-trust nil
      mml2015-cache-passphrase t
      mml2015-passphrase-cache-expiry '36000
      mml2015-sign-with-sender t

      gnus-message-replyencrypt t
      gnus-message-replysign t
      gnus-message-replysignencrypted t
      gnus-treat-x-pgp-sig t

      mm-sign-option 'guided
      mm-encrypt-option 'guided
      mm-verify-option 'always
      mm-decrypt-option 'always

      gnus-buttonized-mime-types
      '("multipart/alternative"
        "multipart/encrypted"
        "multipart/signed")

      epg-debug t ;;  then read the *epg-debug*" buffer
     )

