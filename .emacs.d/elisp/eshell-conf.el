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

;; Also complete according to lisp functions
(setq eshell-show-lisp-completions t)


(defvar eshell-before-wconf nil
  "Window configuration recorded by `eshell-in-other-window'
and restored by `eshell-exit'.")


;; Small BUG: `pop-to-buffer' can return a buffer from another
;; perspective.  In this case, this buffer is also added to the
;; current perspective but the user will probably not notice it
;; immediately.

;;;###autoload
(defun eshell-in-other-window ()
  "Save window configuration and start eshell in other window."
  (interactive)
  (let ((persp-name (if (and (require 'perspective nil t)
                             persp-curr)
                        (concat " <" (persp-name persp-curr) ">")
                      "")
                    )
        ;; Save the pwd because the buffer returned by (pop-to-buffer
        ;; nil) might use another one
        (eshell-in-other-window-pwd default-directory))
    (setq eshell-before-wconf (current-window-configuration))
    (with-current-buffer (pop-to-buffer nil)
      (let ((eshell-buffer-name (concat "*eshell*" persp-name))
            (default-directory eshell-in-other-window-pwd))
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

;; Hide password prompt
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)





;; Eshell automatically adds "cd" before a single-word command if it
;; is a directory.  The following code performs the same for
;; "find-file" before readable files This works even for files ouside
;; the current directory.  However, I have not yet hacked the
;; completion mechanism (advice `eshell-complete-commands-list'?).

(defun my-eshell-file-readable-p (file args)
  "Return non-nil if FILE is a readable file and ARGS is nil.

This is intended to be used as car in
`eshell-interpreter-alist'."
  (and (null args) (file-readable-p file)))

(defun my-eshell-find-file (&rest args)
  "Throw an `eshell-replace-command' exception prefixing ARGS with `find-file'.

This is intended to be used as the cdr corresponding to
`my-eshell-file-readable-p' in `eshell-interpreter-alist'."
  (throw 'eshell-replace-command
         (eshell-parse-command "find-file" (eshell-flatten-list args))))

(defun my-eshell-find-file-hook ()
  "Hook for making Eshell accept single files and prepend `find-file' automatically."
  (add-to-list 'eshell-interpreter-alist
               '(my-eshell-file-readable-p . my-eshell-find-file) 'append))

(provide 'eshell-conf)
;;; eshell-conf.el ends here
