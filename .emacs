;; .emacs, GNU Emacs config file
;; see also .gnus.el for gnus news and mail reader config file

;; Extra lisp libs
(add-to-list 'load-path "~/elisp/") ;; Downloaded packages
;(add-to-list 'load-path "~/elisp/rudel/") ;; For rudel
;(add-to-list 'load-path "~/elisp/rudel/obby/") ;; For rudel
(add-to-list 'load-path "~/.emacs.d/elpa/") ;; Installed packages
(add-to-list 'load-path "~/.emacs.d/elisp/") ;; Configuration
(add-to-list 'load-path "~/.emacs.d/elisp/perspective/")
(add-to-list 'load-path "~/wicd-mode/")
; (require 'wicd-mode)

;; (when (require 'package nil t)
;;   (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;;   (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
;;   (package-initialize))

(require 'perspective)
(persp-mode 1)

(require 'desktop-conf)


;;; Display

;; Customizations depending on wheter emacs is in tty.
;; tty are recognized by the number of available colors.
;(setq ttyp (= (display-color-cells) 8))
(setq ttyp (not (display-graphic-p)))

(when (display-graphic-p)
  (require 'color-theme-conf)
  (require 'windows-conf))

(require 'resize)

;;; Bars and lines

;; Menu bar
(if (display-graphic-p)
    (eval-after-load "menu-bar" '(require 'menu-bar+))
  (menu-bar-mode 0) )

;; Tool bar
(when (display-graphic-p)
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
(global-visual-line-mode 1) ;; wrap long lines on words

;; Minibuffer
(setq minibuffer-auto-raise t
      minibuffer-prompt-properties '(read-only t
                                     point-entered minibuffer-avoid-prompt
                                     face minibuffer-prompt) )

;; Cursor
(when (display-graphic-p)
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
(require 'auto-complete)
(add-hook 'tuareg-mode-hook 'auto-complete-mode)
; (partial-completion-mode 1)

;;; Applications

(require 'erc-conf)
(require 'emms-conf)
(require 'eshell-conf)

;; Couleurs du terminal dans le buffer *Shell Command Output*
(require 'ansi-color)
(defadvice display-message-or-buffer (before ansi-color activate)
  "Process ANSI color codes in shell output."
  (let ((buf (ad-get-arg 0)))
    (and (bufferp buf)
         (string= (buffer-name buf) "*Shell Command Output*")
         (with-current-buffer buf
           (ansi-color-apply-on-region (point-min) (point-max))))))
;; Et dans le buffer de compilation
(defun colorize-compilation-buffer ()
  (let ((buffer-read-only))
    (ansi-color-apply-on-region (point-min) (point-max))
    ))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)



;(require 'rudel-conf)
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
(require 'editing-conf)

;; Printing

(require 'printing)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(backup-directory-alist (quote ((".*" . "./.bkp/"))))
 '(canlock-password "fa237fe1f7828a11d0a39636eeb88b2e97d29993")
 '(column-number-mode t)
 '(coq-load-path (quote ("~/pkg/focalize-0.6.0/lib/zenon-0.6.2/" "/usr/local/lib/focalizec-0.6.0/")))
 '(coq-prog-name "/home/cauderlier/pkg/focalize-0.6.0/bin/coqtop")
 '(dired-listing-switches "-lrth --time-style=+%D%6R")
 '(ede-project-directories (quote ("/home/harry/wicd-mode")))
 '(glasses-uncapitalize-p t)
 '(global-reveal-mode t)
 '(indent-tabs-mode nil)
 '(initial-scratch-message nil)
 '(list-directory-verbose-switches "-l")
 '(makefile-electric-keys t)
 '(org-agenda-files (quote ("~/org/todo.org")))
 '(org-export-latex-default-packages-alist (quote (("" "fixltx2e" nil) ("" "graphicx" t) ("" "longtable" nil) ("" "float" nil) ("" "wrapfig" nil) ("" "soul" t) ("" "textcomp" t) ("" "marvosym" t) ("" "wasysym" t) ("" "latexsym" t) ("" "amssymb" t) ("" "hyperref" nil) "\\tolerance=1000")))
 '(org-export-latex-packages-alist nil)
 '(org-latex-to-pdf-process (quote ("xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f")))
 '(org-modules (quote (org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m org-special-blocks)))
 '(read-mail-command (quote gnus))
 '(recentf-mode t)
 '(safe-local-variable-values (quote ((coq-prog-name . "~/pkg/focalize-0.6.0/bin/coqtop"))))
 '(show-paren-mode t)
 '(underline-minimum-offset 0)
 '(visible-bell t)
 '(which-function-mode t)
 '(wicd-wireless-filter ".*"))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face :foreground "dark green"))))
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
