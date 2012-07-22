;; .emacs, GNU Emacs config file
;; see also .gnus.el for gnus news and mail reader config file

;; Extra lisp libs
(add-to-list 'load-path "~/elisp/") ;; Downloaded packages
(add-to-list 'load-path "~/elisp/rudel/") ;; For rudel
(add-to-list 'load-path "~/elisp/rudel/obby/") ;; For rudel
(add-to-list 'load-path "~/.emacs.d/elisp/") ;; Configuration

(require 'desktop-conf)


;;; Display

;; Customizations depending on wheter emacs is in tty.
;; tty are recognized by the number of available colors.
(setq ttyp (= (display-color-cells) 8))

(unless ttyp
  (require 'color-theme-conf)
  (require 'windows-conf))

(require 'resize)

;;; Bars and lines

;; Menu bar
(if ttyp
    (menu-bar-mode 0)
  (eval-after-load "menu-bar" '(require 'menu-bar+)) )

;; Tool bar
(unless ttyp
  (add-hook 'after-change-major-mode-hook
            (lambda () (tool-bar-mode 0))) )

;; Mode line
(column-number-mode 1)

;; Header line
(require 'header-line-conf)

;;; Buffers

;; uniquify buffer names according to their files paths
;(setq uniquify-buffer-name-style 'forward nil (uniquify)) 
;; update buffer contents when their files change
(global-auto-revert-mode 1)

;; Minibuffer
(setq minibuffer-auto-raise t
      minibuffer-prompt-properties '(read-only t
                                     point-entered minibuffer-avoid-prompt
                                     face minibuffer-prompt) )

;; Cursor
(unless ttyp
  ;; (when (require 'bar-cursor nil t)
  ;;   (bar-cursor-mode 1) )
  ;; (blink-cursor-mode 1)
  ;; (setq x-stretch-cursor t) );; if a block cursor is over a tab, it will be drawn as wide as that tab on the display
  (setq cursor-type 'bar))

;; Completion
(setq completion-auto-help 'lazy
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t )
(icomplete-mode 1)
(partial-completion-mode 1)

;;; Applications

(require 'erc-conf)
(require 'emms-conf)
(require 'eshell-conf)
(require 'rudel-conf)
(require 'org-conf)

;; Web browser

;; firefox is iceweasel on Debian
(let ((file "/usr/bin/iceweasel"))
  (and (file-exists-p file) (file-executable-p file) (setq browse-url-firefox-programm "iceweasel")))
(if (and ttyp (featurep 'w3m))
    (progn
      (setq browse-url-browser-function 'w3m-browse-url)
      (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t))
  (setq browse-url-browser-function 'browse-url-default-browser))

;; File browser
(setq dired-auto-revert-buffer t
      dired-dwim-target t ;; guess default target dir
      dired-listing-switches "-lrth" ;; options passed to ls
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

(require 'latex-conf)
(require 'focalize)
(require 'dedukti)
(require 'tuareg-conf)
;(require 'isabelle)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(backup-directory-alist (quote ((".*" . "./.bkp/"))))
 '(canlock-password "fa237fe1f7828a11d0a39636eeb88b2e97d29993")
 '(column-number-mode t)
 '(glasses-uncapitalize-p t)
 '(global-reveal-mode t)
 '(indent-tabs-mode nil)
 '(initial-scratch-message nil)
 '(makefile-electric-keys t)
 '(read-mail-command (quote gnus))
 '(recentf-mode t)
 '(show-paren-mode t)
 '(underline-minimum-offset 0)
 '(visible-bell t)
 '(which-function-mode t))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(isabelle-quote-face ((((type x) (class color) (background light)) (:foreground "red"))))
 '(isabelle-string-face ((((type x) (class color) (background light)) (:background "lightblue" :foreground "springgreen4"))))
 '(unicode-tokens-fraktur-font-face ((t (:slant normal :weight normal :height 120 :width normal :foundry "bitstream" :family "IsabelleText"))))
 '(unicode-tokens-script-font-face ((t (:slant italic :weight normal :height 113 :width normal :foundry "unknown" :family "LMMathSymbols6"))))
 '(unicode-tokens-symbol-font-face ((t (:slant normal :weight normal :height 98 :width normal :foundry "bitstream" :family "IsabelleText")))))

;(global-set-key "\C-c\C-j" 'proof-goto-point)
;(proofgeneral)


(require 'init-actions nil t)

; (require 'apt)


;; other interesting emacs features :
;; SES : tableur en elisp
;; image modes
;; dired
;; vc-mode -> Version Control
;; org-mode -> TODO lists
;; WoMan