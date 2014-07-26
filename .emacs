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
(add-to-list 'load-path "~/.emacs.d/elpa/auctex-11.87.3/") ; Should not be necessary

(defmacro my-with-persp (name &rest body)
  "Switch to the perspective given by NAME and evaluate BODY.
If the perspective NAME doesn't yet exists, create it.
If the perspective library is not available, just evaluate BODY."
  (if (fboundp 'persp-mode)             ; persp library available
      `(progn
         (unless persp-mode (persp-mode))
         (persp-switch ,name)
         ,@body)
    body))

(defmacro define-persp-app (persp-name form &optional key first-form)
  "Define a command persp- PERSP-NAME by wrapping FORM by `my-with-persp'.
If KEY is non nil, bind it to this command.
If FIRST-FORM is non nil,
call it before FORM when perspective is created."
  (let ((persp-command (intern (concat "persp-" persp-name))))
    (list 'progn
     `(defun ,persp-command ()
        ,(format "Run %s in a dedicated perspective." persp-name)
        (interactive)
        (my-with-persp ,persp-name
                       ,(if first-form
                          `(unless (and (fboundp 'persp-names)
                                        (member ,persp-name (persp-names)))
                             ,first-form ,form)
                          form)))
     (when key
       `(global-set-key ,key ',persp-command)))))

(define-persp-app "packages" (list-packages) (kbd "C-c p"))

;; Package management
(eval-after-load "package"
  '(setq package-archives
         '(("gnu" . "http://elpa.gnu.org/packages/")
           ("org" . "http://orgmode.org/elpa/")
           ("melpa" . "http://melpa.milkbox.net/packages/")
           ("marmalade" . "http://marmalade-repo.org/packages/"))))

;; History
(require 'desktop-conf)

;;; Display

;; Customizations depending on wheter Emacs is in tty.
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (if (display-graphic-p frame)
                (require 'graphic-conf)
              (require 'tty-conf))))

(autoload 'v-resize "resize" 'interactive nil) ; resize windows with C-c +
(global-set-key (kbd "C-c +") 'v-resize)

;; Mode line
(column-number-mode 1)

;; Header line
(require 'header-line-conf)

;;; Buffers

(global-auto-revert-mode 1)     ; update buffer contents when their files change
(global-visual-line-mode 1)     ; wrap long lines on words

;; Buffers listing
;; Rebind C-x C-b to ibuffer, an improved buffer list
(define-persp-app "ibuffer" (ibuffer) (kbd "C-x C-b"))

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

(define-persp-app "erc" () (kbd "C-c i") (erc))

(eval-after-load "emms"
  '(require 'emms-conf))

(define-persp-app "emms" (emms) (kbd "C-c m")
  (emms-play-playlist "~/Musique/playlist"))

(autoload 'eshell-in-other-window "eshell-conf")
(global-set-key (kbd "<s-return>") 'eshell-in-other-window)

(define-persp-app "eshell" (eshell) (kbd "C-c s"))

;; Activate compilation-shell-minor-mode to jump to files
(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

(eval-after-load "eshell"
  '(require 'eshell-conf))

(eval-after-load "rudel"
  '(require 'rudel-conf))

(define-persp-app "agenda" (org-agenda) (kbd "C-c a"))

(eval-after-load "org"
  '(require 'org-conf))

;; Web browser

;; firefox is iceweasel on Debian
(let ((file "/usr/bin/iceweasel"))
  (and (file-exists-p file)
       (file-executable-p file)
       (setq browse-url-firefox-programm "iceweasel")))

(setq browse-url-browser-function 'browse-url-default-browser)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." 'interactive)
(eval-after-load "w3m"
  '(setq browse-url-browser-function 'w3m-browse-url
         w3m-search-default-engine "duckduckgo"
         w3m-search-engine-alist
         '(("debian-pkg"
            "http://packages.debian.org/cgi-bin/search_contents.pl?word=%s"
            nil)
           ("debian-bts"
            "http://bugs.debian.org/cgi-bin/pkgreport.cgi?archive=yes&pkg=%s"
            nil)
           ("emacswiki"
            "http://www.emacswiki.org/cgi-bin/wiki?search=%s"
            nil)
           ("wikipedia-en"
            "http://en.wikipedia.org/wiki/Special:Search?search=%s"
            nil)
           ("wikipedia-fr"
            "http://fr.wikipedia.org/wiki/Special:Search?search=%s"
            utf-8)
           ("ja.wikipedia"
            "http://ja.wikipedia.org/wiki/Special:Search?search=%s"
            utf-8)
           ("duckduckgo" "https://duckduckgo.com/?q=%s" utf-8)
           ("wiki"
            "http://wiki.crans.org/?action=fullsearch&value=%s"
            utf-8)
           ("wikoeur"
            "http://pimeys.fr/wikoeur/?action=fullsearch&value=%s"
            utf-8))))

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
(global-set-key (kbd "C-c g") 'magit-status)

;; Pairs matching
(setq electric-pair-pairs '((?\" . ?\")
                            (?\( . ?\))
                            (?\{ . ?\})
                            (?\[ . ?\])))

(define-persp-app "wicd" (wicd) (kbd "C-c w"))

(define-persp-app "xkcd" (xkcd) (kbd "C-c x"))

(require 'zone)
(zone-when-idle 120)

(when (require 'hungry-delete nil t)
  (global-hungry-delete-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(backup-directory-alist (quote ((".*" . "./.bkp/"))))
 '(default-input-method "TeX")
 '(delete-selection-mode t)
 '(dired-listing-switches "-lrth --time-style=+%D%6R")
 '(ede-project-directories (quote ("/home/harry/wicd-mode")))
 '(electric-indent-mode t)
 '(electric-pair-mode t)
 '(eshell-visual-commands (quote ("vi" "screen" "top" "less" "more" "lynx" "ncftp" "pine" "tin" "trn" "elm" "htop" "wicd-curses")))
 '(indent-tabs-mode nil)
 '(initial-scratch-message nil)
 '(list-directory-verbose-switches "-l")
 '(makefile-electric-keys t)
 '(package-archive-exclude-alist (quote (("melpa" org))))
 '(read-mail-command (quote gnus))
 '(recentf-mode t)
 '(safe-local-variable-values (quote ((encoding . utf-8))))
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
 '(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face :foreground "dark green")))))

(require 'init-actions nil t)

(defun my-startup ()
  "Visit my startup file."
  (interactive)
  (my-with-persp "main" (find-file "~/org/startup.org")))

(global-set-key (kbd "<menu>") 'my-startup)

;; Per-host, unversionized configuration
(require 'local-conf nil t)

;; other interesting emacs features :
;; SES : tableur en elisp
;; image modes
;; dired
;; vc-mode -> Version Control
;; org-mode -> TODO lists
;; WoMan

;;; .emacs ends here
