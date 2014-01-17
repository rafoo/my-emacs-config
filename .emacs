;; .emacs, GNU Emacs config file
;; see also .gnus.el for gnus news and mail reader config file

;; Load-path
(add-to-list 'load-path "~/elisp/") ;; Downloaded packages
(add-to-list 'load-path "~/.emacs.d/elpa/") ;; Installed packages
(add-to-list 'load-path "~/.emacs.d/elisp/") ;; Configuration

;; Package management
(when (require 'package nil t)
  (setq package-archives
        '(("gnu" . "http://elpa.gnu.org/packages/")
          ("org" . "http://orgmode.org/elpa/")
          ("melpa" . "http://melpa.milkbox.net/packages/")))
  (package-initialize))

(global-set-key (kbd "C-c p") 'list-packages)

;; History
(require 'desktop-conf)

;;; Display

;; Customizations depending on wheter emacs is in tty.
(if (display-graphic-p)
    (require 'graphic-conf)
  (require 'tty-conf))

(require 'resize)

;; Mode line
(column-number-mode 1)

;; Header line
(require 'header-line-conf)

;;; Buffers

;; update buffer contents when their files change
(global-auto-revert-mode 1)
(global-visual-line-mode 1) ;; wrap long lines on words

;; Minibuffer
(setq minibuffer-auto-raise t
      minibuffer-prompt-properties '(read-only t
                                     point-entered minibuffer-avoid-prompt
                                     face minibuffer-prompt) )

;; Completion
(setq completion-auto-help 'lazy
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t )
(icomplete-mode 1)

(require 'ido)
(ido-mode t)

(when (require 'discover nil t)
  (global-discover-mode 1))

;;; Applications

(require 'erc-conf)
(require 'emms-conf)
(require 'eshell-conf)

;(require 'rudel-conf)
(require 'org-conf)

;; Web browser

;; firefox is iceweasel on Debian
(let ((file "/usr/bin/iceweasel"))
  (and (file-exists-p file)
       (file-executable-p file)
       (setq browse-url-firefox-programm "iceweasel")))

(if (require 'w3m nil t)
    (progn
      (setq browse-url-browser-function 'w3m-browse-url)
      (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t))
  (setq browse-url-browser-function 'browse-url-default-browser))

;; File browser
(setq dired-auto-revert-buffer t
      dired-dwim-target t ;; guess default target dir
)



;; Spell checking
(setq ispell-program-name "aspell")

;; Net-utils
(setq arp-program "/usr/sbin/arp"
      ifconfig-program "/sbin/ifconfig"
      iwconfig-program "/sbin/iwconfig"
      iwconfig-program-options '("wlan0")
      iwlist-program "/sbin/iwlist"
      iwlist-program-options '("wlan0" "scan"))

;; Editing
(require 'editing-conf)

;; Git
(when (require 'magit nil t)
  (global-set-key (kbd "C-c m") 'magit-status))

;; Printing

(require 'printing)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist (quote ((".*" . "./.bkp/"))))
 '(coq-load-path (quote ("/usr/local/lib/focalize" "/usr/local/lib/zenon")))
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes (quote ("f3d2144fed1adb27794a45e61166e98820ab0bbf3cc7ea708e4bf4b57447ee27" "fc6e906a0e6ead5747ab2e7c5838166f7350b958d82e410257aeeb2820e8a07a" "3d6b08cd1b1def3cc0bc6a3909f67475e5612dba9fa98f8b842433d827af5d30" "f61972772958e166cda8aaf0eba700aad4faa0b4101cee319e894e7a747645c9" default)))
 '(delete-selection-mode t)
 '(dired-listing-switches "-lrth --time-style=+%D%6R")
 '(ede-project-directories (quote ("/home/harry/wicd-mode")))
 '(electric-indent-mode t)
 '(electric-pair-mode t)
 '(electric-pair-pairs (quote ((?\" . ?\") (?\( . ?\)) (?\{ . ?\}) (?\[ . ?\]))))
 '(indent-tabs-mode nil)
 '(initial-scratch-message nil)
 '(list-directory-verbose-switches "-l")
 '(makefile-electric-keys t)
 '(org-agenda-files (quote ("~/zamok/org/todo.org.gpg" "~/org/todo.org")))
 '(org-agenda-span 14)
 '(org-export-latex-default-packages-alist (quote (("" "fixltx2e" nil) ("" "graphicx" t) ("" "longtable" nil) ("" "float" nil) ("" "wrapfig" nil) ("" "soul" t) ("" "textcomp" t) ("" "marvosym" t) ("" "wasysym" t) ("" "latexsym" t) ("" "amssymb" t) ("" "hyperref" nil) "\\tolerance=1000")))
 '(org-export-latex-packages-alist nil)
 '(org-latex-default-packages-alist (quote (("" "fixltx2e" nil) ("" "graphicx" t) ("" "longtable" nil) ("" "float" nil) ("" "wrapfig" nil) ("" "soul" t) ("" "textcomp" t) ("" "marvosym" t) ("" "wasysym" t) ("" "latexsym" t) ("" "amssymb" t) ("" "hyperref" nil) "\\tolerance=1000")))
 '(org-latex-packages-alist nil)
 '(org-latex-pdf-process (quote ("xelatex -interaction nonstopmode -output-directory %o %f" "bibtex %b" "xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f")))
 '(org-latex-to-pdf-process (quote ("xelatex -interaction nonstopmode -output-directory %o %f" "bibtex %b" "xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f")))
 '(org-modules (quote (org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m org-special-blocks)))
 '(org-src-fontify-natively t)
 '(package-archive-exclude-alist (quote (("melpa" org))))
 '(read-mail-command (quote gnus))
 '(recentf-mode t)
 '(safe-local-variable-values (quote ((coq-prog-name . "~/pkg/focalize-0.6.0/bin/coqtop"))))
 '(show-paren-mode t)
 '(underline-minimum-offset 0)
 '(visible-bell t)
 '(w3m-search-default-engine "duckduckgo")
 '(w3m-search-engine-alist (quote (("debian-pkg" "http://packages.debian.org/cgi-bin/search_contents.pl?directories=yes&arch=i386&version=unstable&case=insensitive&word=%s" nil) ("debian-bts" "http://bugs.debian.org/cgi-bin/pkgreport.cgi?archive=yes&pkg=%s" nil) ("emacswiki" "http://www.emacswiki.org/cgi-bin/wiki?search=%s" nil) ("wikipedia-en" "http://en.wikipedia.org/wiki/Special:Search?search=%s" nil) ("wikipedia-fr" "http://fr.wikipedia.org/wiki/Special:Search?search=%s" utf-8) ("ja.wikipedia" "http://ja.wikipedia.org/wiki/Special:Search?search=%s" utf-8) ("duckduckgo" "https://duckduckgo.com/?q=%s" utf-8) ("wiki" " http://wiki.crans.org/?action=fullsearch&value=%s&titlesearch=Titres" utf-8) ("wikoeur" "http://pimeys.fr/wikoeur/?action=fullsearch&value=%s&titlesearch=Titres" utf-8))))
 '(which-function-mode t)
 '(wicd-wireless-filter ".*"))

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
  (find-file "~/org/startup.org"))

(global-set-key (kbd "<menu>") 'my-startup)

;; other interesting emacs features :
;; SES : tableur en elisp
;; image modes
;; dired
;; vc-mode -> Version Control
;; org-mode -> TODO lists
;; WoMan
