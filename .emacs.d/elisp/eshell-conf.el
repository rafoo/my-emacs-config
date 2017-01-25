;;; eshell-conf.el -- configuration for Eshell

;;; Commentary:
;; Not part of Emacs.

;;; Code:

(setq eshell-modules-list '(eshell-alias
                            eshell-basic
                            eshell-cmpl
                            eshell-dirs
                            eshell-glob
                            eshell-hist
                            eshell-ls
                            eshell-pred
                            eshell-prompt
                            eshell-script
                            eshell-term
                            eshell-unix))

(defvar eshell-before-wconf nil
  "Window configuration recorded by `eshell-in-other-window'
and restored by `eshell-exit'.")

(defun eshell-in-other-window ()
  "Save window configuration and start eshell in other window."
  (interactive)
  (let ((persp-name (if (and (require 'perspective nil t)
                             persp-curr)
                        (concat " <" (persp-name persp-curr) ">")
                      "")))
    (setq eshell-before-wconf (current-window-configuration))
    (with-current-buffer (pop-to-buffer nil)
      (let ((eshell-buffer-name (concat "*eshell*" persp-name)))
        (eshell)))))

(defun eshell-exit ()
  "Exit Eshell and restore the previous window configuration."
  (interactive)
  (when eshell-before-wconf
    (set-window-configuration eshell-before-wconf))
  (setq eshell-before-wconf nil))

(defun eshell-exit-when-eolp ()
  "Exit Eshell if point is at end of line.
Otherwise delete one character."
  (interactive)
  (if (eolp)
      (eshell-exit)
    (delete-char 1)))

(defun eshell-C-d-hook ()
  ;; I don't know how to do this whitout local-set-key
  ;; because eshell-mode-map is buffer-local
  ;; (and I don't know why).
  (local-set-key
   (kbd "C-d")
   'eshell-exit-when-eolp))

;; C-d in eshell exit
(add-hook 'eshell-mode-hook #'eshell-C-d-hook)

(provide 'eshell-conf)
;;; eshell-conf.el ends here
