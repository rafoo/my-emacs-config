;;; persp-conf.el --- Functions for manipulating perspectives
;;; Commentary:
;; This is not part of Emacs.
;;; Code:

;; Custom Perspectives

(eval-when-compile
  (require 'perspective))

(defun my-persp-switch ()
  "Wrapper aroud `persp-switch' activating `persp-mode'."
  (interactive)
  (unless persp-mode (persp-mode))
  (persp-switch nil)
  )

(define-persp-app "config"
  (progn
    (find-file "~/git/emacs-config/.emacs")
    (find-file "~/elisp/local-conf.el")
    (dired "~/git/emacs-config/.emacs.d/elisp")
    (magit-status-setup-buffer "~/git/emacs-config"))
  (kbd "C-c <menu>"))

(define-persp-app "packages" (list-packages) (kbd "C-c p"))
(define-persp-app "gnus" (gnus) (kbd "C-c n")) ;; News
(define-persp-app "erc" () (kbd "C-c i") (erc)) ;; IRC
(define-persp-app "agenda" (org-agenda) (kbd "C-c a"))
(define-persp-app "org" (dired "~/git/org") (kbd "C-c o"))
(eval-and-compile
  (when (require 'xkcd nil t)
    (define-persp-app "xkcd" (xkcd) (kbd "C-c x"))))
; (define-persp-app "main" (find-file "~/org/startup.org") (kbd "<menu>"))

;; Buffers listing
;; Rebind C-x C-b to ibuffer, an improved buffer list
(define-persp-app "ibuffer" (ibuffer) (kbd "C-x C-b"))

(define-persp-with-shell-process "wicd" "wicd-gtk" (kbd "C-c w"))
(define-persp-with-shell-process "tbb" "tbb" (kbd "C-c b"))

;; Overide persp-delete-frame that is supposed to be called to clear a
;; closing frame but is in fact causing trouble in other frames too.
(defun persp-delete-frame (frame)
  "Ignore the FRAME."
  nil)

(provide 'persp-conf)
;;; persp-conf.el ends here
