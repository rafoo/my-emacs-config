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

(defun nnmaildir--scan (gname scan-msgs groups method srv-dir srv-ls)
  (catch 'return
    (let ((36h-ago (- (car (current-time)) 2))
          (index (- (length groups) (length (memq gname dirs))))
	  absdir nndir tdir ndir cdir nattr cattr isnew pgname read-only ls
	  files num dir flist group x)
      (setq absdir (nnmaildir--srvgrp-dir srv-dir gname)
	    nndir (nnmaildir--nndir absdir))
      (unless (file-exists-p absdir)
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "No such directory: " absdir))
	(throw 'return nil))
      (setq tdir (nnmaildir--tmp absdir)
	    ndir (nnmaildir--new absdir)
	    cdir (nnmaildir--cur absdir)
	    nattr (file-attributes ndir)
	    cattr (file-attributes cdir))
      (unless (and (file-exists-p tdir) nattr cattr)
	(setf (nnmaildir--srv-error nnmaildir--cur-server)
	      (concat "Not a maildir: " absdir))
	(throw 'return nil))
      (setq group (nnmaildir--prepare nil gname)
	    pgname (nnmaildir--pgname nnmaildir--cur-server gname))
      (if group
	  (setq isnew nil)
	(setq isnew t
	      group (make-nnmaildir--grp :name gname :index 0))
	(nnmaildir--mkdir nndir)
	(nnmaildir--mkdir (nnmaildir--nov-dir   nndir))
	(nnmaildir--mkdir (nnmaildir--marks-dir nndir)))
      (setq read-only (nnmaildir--param pgname 'read-only)
	    ls (or (nnmaildir--param pgname 'directory-files) srv-ls))
      (unless read-only
	(setq x (nth 11 (file-attributes tdir)))
	(unless (and (equal x (nth 11 nattr)) (equal x (nth 11 cattr)))
	  (setf (nnmaildir--srv-error nnmaildir--cur-server)
		(concat "Maildir spans filesystems: " absdir))
	  (throw 'return nil))
	(dolist (file (funcall ls tdir 'full "\\`[^.]" 'nosort))
	  (setq x (file-attributes file))
	  (if (or (> (cadr x) 1) (< (car (nth 4 x)) 36h-ago))
	      (delete-file file))))
      (or scan-msgs
	  isnew
	  (throw 'return t))
      (setq nattr (nth 5 nattr))
      (if (equal nattr (nnmaildir--grp-new group))
	  (setq nattr nil))
      (if read-only (setq dir (and (or isnew nattr) ndir))
	(when (or isnew nattr)
	  (dolist (file  (funcall ls ndir nil "\\`[^.]" 'nosort))
	    (setq x (concat ndir file))
	    (and (time-less-p (nth 5 (file-attributes x)) (current-time))
		 (rename-file x (concat cdir (nnmaildir--ensure-suffix file)))))
	  (setf (nnmaildir--grp-new group) nattr))
	(setq cattr (nth 5 (file-attributes cdir)))
	(if (equal cattr (nnmaildir--grp-cur group))
	    (setq cattr nil))
	(setq dir (and (or isnew cattr) cdir)))
      (unless dir (throw 'return t))
      (setq files (funcall ls dir nil "\\`[^.]" 'nosort)
	    files (save-match-data
		    (mapcar
		     (lambda (f)
		       (string-match "\\`\\([^:]*\\)\\(\\(:.*\\)?\\)\\'" f)
		       (cons (match-string 1 f) (match-string 2 f)))
		     files)))
      (when isnew
	(setq num (nnmaildir--up2-1 (length files)))
	(setf (nnmaildir--grp-flist group) (make-vector num 0))
	(setf (nnmaildir--grp-mlist group) (make-vector num 0))
	(setf (nnmaildir--grp-mmth group) (make-vector 1 0))
	(setq num (nnmaildir--param pgname 'nov-cache-size))
	(if (numberp num) (if (< num 1) (setq num 1))
	  (setq num 16
		cdir (nnmaildir--marks-dir nndir)
		ndir (nnmaildir--subdir cdir "tick")
		cdir (nnmaildir--subdir cdir "read"))
	  (dolist (prefix-suffix files)
	    (let ((prefix (car prefix-suffix))
		  (suffix (cdr prefix-suffix)))
	      ;; increase num for each unread or ticked article
	      (when (or
		     ;; first look for marks in suffix, if it's valid...
		     (when (and (stringp suffix)
				(gnus-string-prefix-p ":2," suffix))
		       (or
			(not (gnus-string-match-p
			      (string (nnmaildir--mark-to-flag 'read)) suffix))
			(gnus-string-match-p
			 (string (nnmaildir--mark-to-flag 'tick)) suffix)))
		     ;; then look in marks directories
		     (not (file-exists-p (concat cdir prefix)))
		     (file-exists-p (concat ndir prefix)))
		(incf num)))))
	(setf (nnmaildir--grp-cache group) (make-vector num nil))
        (let ((inhibit-quit t))
          (set (intern gname groups) group))
	(or scan-msgs (throw 'return t)))
      (setq flist (nnmaildir--grp-flist group)
	    files (mapcar
		   (lambda (file)
		     (and (null (nnmaildir--flist-art flist (car file)))
			  file))
		   files)
	    files (delq nil files)
	    files (mapcar 'nnmaildir--parse-filename files)
	    files (sort files 'nnmaildir--sort-files))
      (setq mycounter 0)
      (dolist (file files)
        (gnus-message 9 (format "[%s/%s] %s: %s / %s" index (length groups) gname mycounter (length files)))
        (setq mycounter (1+ mycounter))
	(setq file (if (consp file) file (aref file 3))
	      x (make-nnmaildir--art :prefix (car file) :suffix (cdr file)))
	(nnmaildir--grp-add-art nnmaildir--cur-server group x))
      (if read-only (setf (nnmaildir--grp-new group) nattr)
	(setf (nnmaildir--grp-cur group) cattr)))
    t))
