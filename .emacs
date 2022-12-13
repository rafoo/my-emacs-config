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

(defvar my-home (getenv "HOME"))

(defun add-to-path (dirname)
  "Add DIRNAME to `exec-path' and env variable PATH."
  (let ((path (getenv "PATH")))
    (add-to-list 'exec-path dirname)
    (unless (string-match-p
             (concat (regexp-quote dirname) ":")
             path)
      (setenv "PATH" (concat dirname ":" path)))))

(use-package opam :config (opam-init))

(defun add-to-path-from-home (dirname)
  "Add DIRNAME to `exec-path' and env variable PATH.
DIRNAME is a path relative to the HOME directory."
  (add-to-path (concat my-home "/" dirname)))

(add-to-path-from-home "bin/opentheory/bin/mlton") ; opentheory (HOL package manager)
; (add-to-path-from-home "bin/veriT-stable2016") ; veriT (SMT solver)
;; Version of veriT needed for SMTCoq
(add-to-path-from-home "bin/veriT/veriT9f48a98")
(add-to-path-from-home "git/imogen/src/main/sml/bin") ; imogen (theorem prover)
(add-to-path-from-home "git/verifast/bin") ; verifast, vfide
; (add-to-path-from-home "git/CVC4/builds/x86_64-unknown-linux-gnu/production/bin") 

;; Version of CVC4 needed for SMTCoq
(add-to-path "/home/cauderlier/git/cvc5/builds/bin")

;; LFSC signatures are needed to use CVC4 with SMTCoq
(setenv "LFSCSIGS" "/home/cauderlier/.opam/4.12.0/.opam-switch/sources/coq-smtcoq.dev+8.13/src/lfsc/tests/signatures/")

(add-to-path-from-home "git/yices2/build/x86_64-pc-linux-gnu-release/bin") ; yices2 (SMT solver)
(add-to-path-from-home "scripts")      ; Small personnal shell scripts


;; Used by the texdoc program called from AucTeX
(setenv "PDFVIEWER" "evince")

;; To use git grep in eshell
(setenv "GIT_PAGER" "")

;; C-z is always typed by accident
(global-unset-key (kbd "C-z"))
;; Xmonad-like bindings
(global-set-key (kbd "<s-tab>") 'other-window)

;; Custom keys

;; Enforce that the `use-package' macro is available.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;; Perspectives
(use-package s
  :ensure t)

(use-package perspective
  :requires s
  :config (require 'rtiling))

(use-package rtiling
  :requires (perspective which-key)
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

(use-package eshell
  :config (require 'eshell-conf))

;; M-RET and C-c s start a new eshell instance in the other window
(use-package eshell-conf
  :bind (("M-RET" . eshell-in-other-window)
         ("C-c s" . eshell-in-other-window)))

;; Activate compilation-shell-minor-mode to jump to files
(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

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
 '(backup-directory-alist '((".*" . "./.bkp/")))
 '(canlock-password "0a42e4942e41dbbd56282757bd18beccbbbcd635")
 '(custom-safe-themes
   '("dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" default))
 '(default-input-method "TeX")
 '(delete-selection-mode t)
 '(dired-listing-switches "-lth --time-style=+%D%6R")
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(gnus-init-file "~/.emacs.d/elisp/.gnus")
 '(indent-tabs-mode nil)
 '(initial-scratch-message nil)
 '(list-directory-verbose-switches "-l")
 '(makefile-electric-keys t)
 '(michelson-client-command
   "~/git/tezos/octez-client --base-dir /tmp/mockup --mode mockup --protocol ProtoALpha")
 '(mm-text-html-renderer 'w3m)
 '(org-agenda-files nil)
 '(package-archive-exclude-alist '(("melpa" org)))
 '(package-selected-packages
   '(diff-hl yaml-mode smart-tab origami tldr slack michelson-mode s forge edit-indirect markdown-mode exec-path-from-shell ido-completing-read+ memoize dmenu use-package auctex bind-key dedukti-mode hydra iedit lua-mode magit org pdf-tools php-mode proof-general smartparens tuareg utop which-key zenburn-theme company-coq exwm forth-mode dired-quick-sort "org" "org" "org" "org" "org" "org" org-lint deferred cl-generic zoom-frm z3-mode wgrep verifast-mode start-menu ssh-agency perspective opam offlineimap menu-bar+ magit-popup ido-vertical-mode ido-at-point htmlize guess-language graphviz-dot-mode flycheck-dedukti diminish define-persp column-enforce-mode auto-highlight-symbol auto-complete))
 '(read-mail-command 'gnus)
 '(recentf-mode t)
 '(safe-local-variable-values
   '((coq-prog-name . "~/.opam/4.07.1/bin/hoqtop")
     (visual-line-mode . 0)
     (visual-line-mode)
     (global-visual-line-mode)))
 '(send-mail-function 'smtpmail-send-it)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(underline-minimum-offset 0)
 '(visible-bell t)
 '(which-function-mode t)
 '(z3-solver-cmd "z3"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face :foreground "dark green"))))
 '(w3m-anchor ((((class color) (background light)) (:foreground "light blue")))))

(require 'init-actions nil t)

;; (defun my-offlineimap-wrapper ()
;;   "Wrapper around offlineimap that does not fail."
;;   (when (require 'offlineimap nil t)
;;     (offlineimap)))

;; (use-package offlineimap
;;   :init
;;   ;; Run offlineimap from time to time
;;   (run-with-idle-timer 60 'repeat 'my-offlineimap-wrapper)
;;   :config
;;   (require 'offlineimap-conf))

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


(use-package slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "tezos-dev"
   :default t
   :subscribed-channels '((general devteam)))
)

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))

;; Per-host, unversionized configuration
;; Alternative: use the sensitive package
(require 'local-conf nil t)


;; other interesting emacs features :
;; SES : tableur en elisp
;; image modes
;; WoMan

;; bookmarks
;; registers

;; org-highlight-latex-and-related

;;;; Packages to try
;;
;;; Completion
;; Ivy
;; Helm, helm-swoop, swiper
;; Pcompelte
;;
;;; Parsers
;; wisi (LALR parser for indentation, fontification, and navigation)
;; rdp (another parser for indentation)
;; peg
;; parsec and emacs-pl (parser combinators)
;; CEDET Semantic (Wisent & Bovine)
;; ParserCompiler (https://www.emacswiki.org/emacs/ParserCompiler)
;;
;;; Project Management
;; projectile
;;
;;; Volume control
;; volume
;; pulseaudio-control
;;
;;; OCaml
;; ocp-indent: use ocamlpro indenter in Tuareg
;; utop: interface au toplevel OCaml utop
;; merlin
;; also try Ecaml (https://blag.bcc32.com)
;;
;;; Other
;; hydra
;; wrap-region (for example to add $ around region in LaTeX)
;; togetherly (collaborative editing)
;; elfeed (RSS the org way)
;; system-packages (frontend for many package managers)
;; secretaria (reminder for org TODOs)
;; repl-toggle
;; pass, passmm, password-store (pass interface)
;; auto-insert
;; org-ehtml: org-html files that are editable in browser
;; narrow-indirect
;; mpages
;; minibuffer-line
;; magithub / magit-gh-pulls
;; magit-find-file
;;
;; nnreddit: (gnus-reddit integration)
;; anaphora, emacs-huskie, emacs-kv, shoes-off
;;
;; Outshine: org-mode sections in program comments (https://orgmode.org/worg/org-tutorials/org-outside-org.html)
;;
;; look at all available packages from A to M (excl.)
;;; .emacs ends here
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
(put 'narrow-to-region 'disabled nil)
