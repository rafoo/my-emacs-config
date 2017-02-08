;;; persp-conf.el --- Functions for manipulating perspectives
;;; Commentary:
;; This is not part of Emacs.
;;; Code:

(require 'define-persp)

;; Custom Perspectives

(global-set-key (kbd "C-c v") 'define-persp-with-git)
(global-set-key (kbd "C-c !") 'define-persp-with-cmd)

(define-persp-app "config"
  (progn
    (find-file "~/.emacs")
    (find-file "~/elisp/local-conf.el")
    (dired "~/git/emacs-config/.emacs.d/elisp")
    (magit-status "~/git/emacs-config"))
  (kbd "C-c <menu>"))

(define-persp-app "packages" (list-packages) (kbd "C-c p"))
(define-persp-app "gnus" (gnus) (kbd "C-c n")) ;; News
(define-persp-app "erc" () (kbd "C-c i") (erc)) ;; IRC
(define-persp-app "agenda" (org-agenda) (kbd "C-c a"))
(define-persp-app "org" (dired (concat my-home "/git/org")) (kbd "C-c o"))
(define-persp-app "xkcd" (xkcd) (kbd "C-c x"))
(define-persp-app "main" (find-file "~/org/startup.org") (kbd "<menu>"))

;; Buffers listing
;; Rebind C-x C-b to ibuffer, an improved buffer list
(define-persp-app "ibuffer" (ibuffer) (kbd "C-x C-b"))

(define-persp-with-shell-process "wicd" "wicd-gtk" (kbd "C-c w"))
(define-persp-with-shell-process "web" "tbb" (kbd "C-c b"))

(provide 'persp-conf)
;;; persp-conf.el ends here
