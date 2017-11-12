;;; .emacs --- GNU Emacs config file
;; see also .gnus.el for gnus news and mail reader config file
;;; Commentary:
;; This is not part of Emacs.
;;; Code:

;; Load-path
(add-to-list 'load-path "~/elisp/")          ; Downloaded packages
(add-to-list 'load-path "~/.emacs.d/elpa/")  ; Installed packages
(add-to-list 'load-path "~/.emacs.d/elisp/") ; Configuration

;; Package management
(eval-after-load "package"
  '(setq package-archives
         '(("gnu" . "http://elpa.gnu.org/packages/")
           ("org" . "http://orgmode.org/elpa/")
           ("melpa" . "http://melpa.org/packages/")
           ; ("marmalade" . "http://marmalade-repo.org/packages/")
	   )))

(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

(package-initialize)

;; Extend path with opam directory

(defvar my-home (getenv "HOME"))

(defun add-to-path (dirname)
  "Add DIRNAME to `exec-path' and env variable PATH."
  (let ((path (getenv "PATH")))
    (add-to-list 'exec-path dirname)
    (unless (string-match-p
             (concat (regexp-quote dirname) ":")
             path)
      (setenv "PATH" (concat dirname ":" path)))))

(when (executable-find "opam")
  (let ((opam-switch (substring (shell-command-to-string "opam switch show") 0 -1)))
    (add-to-path
     (concat my-home "/.opam/" opam-switch "/bin"))
    ))

(defun add-to-path-from-home (dirname)
  "Add DIRNAME to `exec-path' and env variable PATH.
DIRNAME is a path relative to the HOME directory."
  (add-to-path (concat my-home "/" dirname)))

(add-to-path-from-home "bin/opentheory/bin/mlton")
(add-to-path-from-home "git/imogen/src/main/sml/bin")
(add-to-path-from-home "git/verifast/bin")
(add-to-path-from-home "git/CVC4/builds/x86_64-unknown-linux-gnu/production/bin")
(add-to-path-from-home "scripts")


;; Used by the texdoc program called from AucTeX
(setenv "PDFVIEWER" "evince")

;; To use git grep in eshell
(setenv "GIT_PAGER" "")

;; C-z is always typed by accident
(global-unset-key (kbd "C-z"))
;; Xmonad-like bindings
(global-set-key (kbd "<s-tab>") 'other-window)

;; Custom keys

;; Use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;; Perspectives

(use-package perspective
  :requires s
  :config
  (require 'rtiling))

(use-package rtiling
  :requires perspective
  :config (require 'rtiling-conf))

(use-package define-persp
  :requires perspective
  :commands (define-persp-app define-persp-with-shell-process)
  :bind
  (("C-c v" . define-persp-with-git)
   ("C-c !" . define-persp-with-cmd))
  :init (require 'persp-conf))

(require 'graphic-conf)

(autoload 'v-resize "resize" 'interactive nil) ; resize windows with C-c +
(global-set-key (kbd "C-c +") 'v-resize)

;; Mode line
(column-number-mode 1)

;; Header line
(require 'header-line-conf)

;;; Buffers
(global-auto-revert-mode 1)     ; update buffer contents when their files change
(global-visual-line-mode 1)     ; wrap long lines on words

;; Minibuffer
(setq minibuffer-auto-raise t
      minibuffer-prompt-properties '(read-only t
                                     point-entered minibuffer-avoid-prompt
                                     face minibuffer-prompt))

(require 'completion-conf)

;; (use-package discover
;;   :config (global-discover-mode 1))

;;; Applications

(use-package jabber
  :bind ("C-c j" . jabber-connect))
(use-package erc
  :config (require 'erc-conf))

;; M-RET and C-c s start a new eshell instance in the other window
(use-package eshell-conf
  :bind (("M-RET" . eshell-in-other-window)
         ("C-c s" . eshell-in-other-window)))

(use-package eshell
  :config (require 'eshell-conf))

;; Activate compilation-shell-minor-mode to jump to files
(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

(use-package rudel
  :config (require 'rudel-conf))

(use-package org
  :config (require 'org-conf))

;; Web browser

(let ((file "~/scripts/tbb"))
  (and (file-exists-p file)
       (file-executable-p file)
       (setq browse-url-firefox-program "tbb")))

(setq browse-url-browser-function 'browse-url-default-browser)

;; File browser
(use-package dired
  :config
  (setq dired-auto-revert-buffer t
        dired-dwim-target t)) ; guess default target dir

;; Net-utils
(use-package net-utils
  :config
  (setq arp-program "/usr/sbin/arp"
        ifconfig-program "/sbin/ifconfig"
        iwconfig-program "/sbin/iwconfig"
        iwconfig-program-options '("wlan0")
        iwlist-program "/sbin/iwlist"
        iwlist-program-options '("wlan0" "scan")))

;; Editing
(require 'editing-conf)

;; Git
(use-package magit
  :bind ("C-c g" . magit-status)
  :config (require 'magit-conf))

;; Dired
(use-package dired
  :config (require 'dired-conf))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(backup-directory-alist (quote ((".*" . "./.bkp/"))))
 '(canlock-password "0a42e4942e41dbbd56282757bd18beccbbbcd635")
 '(custom-safe-themes
   (quote
    ("dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" default)))
 '(default-input-method "TeX")
 '(delete-selection-mode t)
 '(gnus-init-file "~/.emacs.d/elisp/.gnus")
 '(indent-tabs-mode nil)
 '(initial-scratch-message nil)
 '(list-directory-verbose-switches "-l")
 '(makefile-electric-keys t)
 '(mm-text-html-renderer (quote w3m))
 '(package-archive-exclude-alist (quote (("melpa" org))))
 '(read-mail-command (quote gnus))
 '(recentf-mode t)
 '(send-mail-function (quote smtpmail-send-it))
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(underline-minimum-offset 0)
 '(visible-bell t)
 '(which-function-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face :foreground "dark green"))))
 '(w3m-anchor ((((class color) (background light)) (:foreground "light blue")))))

(require 'init-actions nil t)

(use-package offlineimap
  :init
  ;; Run offlineimap from time to time
  (run-with-idle-timer 60 'repeat 'offlineimap)
  :config
  (require 'offlineimap-conf))

;; Per-host, unversionized configuration
(require 'local-conf nil t)

;; Enable which-key
(use-package which-key
  :config (which-key-mode t))

;; EXWM
(use-package exwm
  :config (require 'exwm-conf))


;; Auto-kill password file ~/passwd.gpg
;; Source: https://stackoverflow.com/questions/15255080/how-to-auto-close-an-auto-encryption-mode-buffer-in-emacs
(run-with-idle-timer 600 t (lambda ()
                            (let ((victim (get-buffer "passwd.gpg")))
                              (when victim (message "Killing buffer %s" (buffer-name victim)
                                                 (kill-buffer victim))))))

(defun kill-current-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer))

(bind-key "C-x k" 'kill-current-buffer)

(defun ssh-add ()
  "Call the ssh-add shell command.

The user is asked for her passphrase.  This function uses
`send-invisible' to hide the passphrase."
  (interactive)
  (start-process-shell-command "ssh-add" "*ssh-add*" "ssh-add")
  (switch-to-buffer "*ssh-add*")
  (send-invisible "SSH Passphrase: ")
  )

(defun gpg-add ()
  "Unlock gpg-agent.

This function is similar to `ssh-add' but for the GPG agent
instead of the SSH agent.  The user is asked for her passphrase.
This function uses `send-invisible' to hide the passphrase.

This function assumes that a gpg-add script is present in PATH.  It can be implemented as follows:

#!/bin/bash

gpg --pinentry-mode loopback --decrypt /path/to/some/encrypted/file.gpg > /dev/null"
  (interactive)
  (start-process-shell-command "gpg-add" "*gpg-add*" "gpg-add")
  (switch-to-buffer "*gpg-add*")
  (send-invisible "GPG Passphrase: ")
  )

;; PDF
;; PDF-tools is a replacement for Docview
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (add-hook 'pdf-view-mode-hook #'pdf-tools-enable-minor-modes))

;; other interesting emacs features :
;; SES : tableur en elisp
;; image modes
;; WoMan

;;; .emacs ends here
