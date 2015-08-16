;; Général

;; Mode novice
(setq gnus-novice-user t)

;; Topic

;; les nouveaux groupes vont dans le topic qui les réclamme
(setq gnus-subscribe-newsgroup-method 'gnus-subscribe-topics)
;; passe en topic-mode dès le lancement
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

; (setq gnus-fetch-old-headers 'some) ;; Reconstruction des threads
(setq gnus-summary-gather-subject-limit 'fuzzy)

;; Vérification des nouveaux messages toutes les minutes
(require 'gnus-demon)
(gnus-demon-add-handler 'gnus-demon-scan-news 1 t)

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
(setq gnus-select-method '(nnimap "Crans"
                                  (nnimap-address "imap.crans.org")
                                  (nnimap-stream ssl)))

;; News
(setq gnus-secondary-select-methods '(;; (nntp "news.crans.org")
				      ;; (nntp "news.gmane.org")
                                      ;; (nnimap "Cnam"
                                      ;;         (nnimap-address "imap.cnam.fr")
                                      ;;         (nnimap-stream ssl))
))

;; Envoi
(setq smtpmail-default-smtp-server "smtp.crans.org")
(setq smtpmail-smtp-server "smtp.crans.org")
(setq send-mail-function 'smtpmail-send-it)
(setq message-send-mail-function 'smtpmail-send-it)
(setq smtpmail-local-domain "crans.org")
(setq smtpmail-sendto-domain "crans.org")
(require 'smtpmail)

;; Display
(gnus-add-configuration
 '(article
   (horizontal 1.0
	       (vertical 50
			 (group 1.0))
	       (vertical 1.0
			 (summary 0.25)
                         (if (featurep 'bbdb) ("*BBDB*" 10))
			 (article 1.0 point)))))

(gnus-add-configuration
 '(summary
   (horizontal 1.0
	       (vertical 50
			 (group 1.0))
	       (vertical 1.0
			 (summary 1.0 point)
                         (if (featurep 'bbdb) ("*BBDB*" 10))
                         ))))

(setq-default gnus-summary-line-format
              (concat "%U%R%z %(%&user-date; %* %B" (if (featurep 'bbdb) "%uB" "%a") "  %s%)\n")
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
(setq mm-discouraged-alternatives '("text/html" "text/richtext"))

;; Posting styles
(setq user-mail-address "cauderlier@crans.org")

(setq gnus-posting-styles '((".*"
                             (address "cauderlier@crans.org"
			      name "Raphaël Cauderlier"))))

(setq message-alternative-emails
      (regexp-opt '("cauderlier@crans.org"
                    "raphael.cauderlier@crans.org"
                    "raphael.cauderlier@inria.fr"
                    "raphael.cauderlier@cnam.fr"
                    "raphael.cauderlier@dptinfo.ens-cachan.fr"
                    "rcauderl@dptinfo.ens-cachan.fr"
                    "raphael.cauderlier@ens-cachan.fr"
                    "rcauderl@ens-cachan.fr"
                    "raphael@interagir.com"
                    "harry@crans.org")))

;; Attachments
(setq mm-enable-external 'ask)

;; nov
(setq nnimap-nov-is-evil t)

;; BBDB
(when (featurep 'bbdb)
  (setq bbdb/mail-auto-create-p t)
  (autoload 'bbdb/send-hook "moy-bbdb"
    "Function to be added to `message-send-hook' to notice records when sending messages" t)
  (add-hook 'message-setup-hook 'bbdb/send-hook)

  (defun display-bbdb-sender ()
    (prin1 "Displaying BBDB for Sender")
    (save-excursion
      (let* ((sender (mail-fetch-field "from"))
             (components (when sender (bbdb-extract-address-components sender))))
        (when components (bbdb-search-mail (cadr components))
              ))))

  (add-hook 'gnus-article-mode-hook 'display-bbdb-sender)
  )
