;;; .emacs --- GNU Emacs config file
;; see also .gnus.el for gnus news and mail reader config file
;;; Commentary:
;; This is not part of Emacs.
;;; Code:

;; Load-path
(add-to-list 'load-path "~/elisp/")          ; Downloaded packages
(add-to-list 'load-path "~/.emacs.d/elpa/")  ; Installed packages
(add-to-list 'load-path "~/.emacs.d/elisp/") ; Configuration
(add-to-list 'load-path "~/git/wicd-mode/")  ; My wicd interface

;; Package management
(eval-after-load "package"
  '(setq package-archives
         '(("gnu" . "http://elpa.gnu.org/packages/")
           ("org" . "http://orgmode.org/elpa/")
           ("melpa" . "http://melpa.org/packages/")
           ("marmalade" . "http://marmalade-repo.org/packages/"))))

(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Extend path with opam directory

(defvar my-home (getenv "HOME"))

(defun add-to-path (dirname)
  "Add DIRNAME to `'exec-path' and env variable PATH."
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
    (eval-after-load 'dedukti-mode
      (setq dedukti-path (concat my-home "/.opam/" opam-switch "/bin/dkcheck"))
    )))


;; Custom keys
(require 'persp-conf)

;; C-z is always typed by accident
(global-unset-key (kbd "C-z"))
;; Xmonad-like bindings
(global-set-key (kbd "<s-tab>") 'other-window)
;; Jabber
(global-set-key (kbd "C-c j") 'jabber-connect)

;;; Display
(package-initialize)
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

;; (when (require 'discover nil t)
;;   (global-discover-mode 1))

;;; Applications

(eval-after-load "erc"
  '(require 'erc-conf))

(autoload 'eshell-in-other-window "eshell-conf")
(global-set-key (kbd "<s-return>") 'eshell-in-other-window)
(global-set-key (kbd "M-RET") 'eshell-in-other-window)
(global-set-key (kbd "C-c s") 'eshell-in-other-window)

;; Activate compilation-shell-minor-mode to jump to files
(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

(eval-after-load "eshell"
  '(require 'eshell-conf))

(eval-after-load "rudel"
  '(require 'rudel-conf))

(eval-after-load "org"
  '(require 'org-conf))

;; Web browser

(let ((file "~/scripts/tbb"))
  (and (file-exists-p file)
       (file-executable-p file)
       (setq browse-url-firefox-program "tbb")))

(setq browse-url-browser-function 'browse-url-default-browser)

;; File browser
(eval-after-load "dired"
  '(setq dired-auto-revert-buffer t
         dired-dwim-target t)) ; guess default target dir

;; Spell checking
(eval-after-load "ispell"
  '(setq ispell-program-name "aspell"))

;; Net-utils
(eval-after-load "net-utils"
  '(setq arp-program "/usr/sbin/arp"
         ifconfig-program "/sbin/ifconfig"
         iwconfig-program "/sbin/iwconfig"
         iwconfig-program-options '("wlan0")
         iwlist-program "/sbin/iwlist"
         iwlist-program-options '("wlan0" "scan")))

;; Editing
(require 'editing-conf)

;; Git
(autoload 'magit-status "magit" "Open a Magit status buffer […]" t nil)
(global-set-key (kbd "C-c g") 'magit-status)
(require 'magit-conf)

(require 'coq-conf)

;; Compilation
; (global-set-key (kbd "C-c c") 'compile)

;; Pairs matching
(setq electric-pair-pairs '((?\" . ?\")
                            (?\( . ?\))
                            (?\{ . ?\})
                            (?\[ . ?\])))

(when (require 'hungry-delete nil t)
  (global-hungry-delete-mode))

;; Ispell
(defun my-ispell-switch-language ()
  "Switch Ispell dictionary between English and French."
  (interactive)
  (ispell-change-dictionary
   (if (string= ispell-current-dictionary "english")
       "french"
     "english"))
  )

(global-set-key (kbd "C-c f") 'my-ispell-switch-language)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-idle-interval 0.2)
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(auth-source-save-behavior nil)
 '(backup-directory-alist (quote ((".*" . "./.bkp/"))))
 '(canlock-password "0a42e4942e41dbbd56282757bd18beccbbbcd635")
 '(custom-safe-themes
   (quote
    ("dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" default)))
 '(dedukti-check-options (quote ("-nc" "-r")))
 '(dedukti-compile-options (quote ("-nc" "-e" "-r")))
 '(default-input-method "TeX")
 '(delete-selection-mode t)
 '(dired-listing-switches "-lrth --time-style=+%D%6R")
 '(ede-project-directories (quote ("/home/harry/wicd-mode")))
 '(electric-pair-mode t)
 '(erc-track-switch-direction (quote importance))
 '(fci-rule-color "#383838")
 '(flyspell-auto-correct-binding (kbd "M-<tab>"))
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
 '(tab-always-indent (quote complete))
 '(underline-minimum-offset 0)
 '(visible-bell t)
 '(w3m-search-default-engine "duckduckgo")
 '(w3m-search-engine-alist
   (quote
    (("debian-pkg" "http://packages.debian.org/cgi-bin/search_contents.pl?directories=yes&arch=i386&version=unstable&case=insensitive&word=%s" nil)
     ("debian-bts" "http://bugs.debian.org/cgi-bin/pkgreport.cgi?archive=yes&pkg=%s" nil)
     ("emacswiki" "http://www.emacswiki.org/cgi-bin/wiki?search=%s" nil)
     ("wikipedia-en" "http://en.wikipedia.org/wiki/Special:Search?search=%s" nil)
     ("wikipedia-fr" "http://fr.wikipedia.org/wiki/Special:Search?search=%s" utf-8)
     ("ja.wikipedia" "http://ja.wikipedia.org/wiki/Special:Search?search=%s" utf-8)
     ("duckduckgo" "https://duckduckgo.com/?q=%s" utf-8)
     ("wiki" " http://wiki.crans.org/?action=fullsearch&value=%s&titlesearch=Titres" utf-8)
     ("wikoeur" "http://pimeys.fr/wikoeur/?action=fullsearch&value=%s&titlesearch=Titres" utf-8))))
 '(wdired-allow-to-change-permissions t)
 '(which-function-mode t)
 '(wicd-wireless-filter ".*"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face :foreground "dark green"))))
 '(w3m-anchor ((((class color) (background light)) (:foreground "light blue")))))

(require 'init-actions nil t)

(require 'offlineimap-conf)

;; Per-host, unversionized configuration
(require 'local-conf nil t)

;; Enable which-key
(when (require 'which-key nil t)
  (which-key-mode t))

;; EXWM
(when (require 'exwm nil t)
  (require 'exwm-conf))

(require 'rtiling nil t)

(when (require 'perspective nil t)
  (require 'rtiling-conf nil t))


;; Auto-kill password file ~/passwd.gpg
;; Source: https://stackoverflow.com/questions/15255080/how-to-auto-close-an-auto-encryption-mode-buffer-in-emacs
(run-with-idle-timer 60 t (lambda ()
                         (let ((victim (get-buffer "passwd.gpg")))
                           (when victim (message "Killing buffer %s" (buffer-name victim)
                                                 (kill-buffer victim))))))

(put 'scroll-left 'disabled nil)

;; PDF
;; PDF-tools is a replacement for Docview
(when (require 'pdf-tools nil t)
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
  (add-hook 'pdf-view-mode-hook #'pdf-tools-enable-minor-modes))

;; other interesting emacs features :
;; SES : tableur en elisp
;; image modes
;; dired
;; vc-mode -> Version Control
;; org-mode -> TODO lists
;; WoMan

;;; .emacs ends here
