;;; persp-conf.el --- Functions for manipulating perspectives
;;; Commentary:
;; This is not part of Emacs.
;;; Code:

(defmacro my-with-persp (name &rest body)
  "Switch to the perspective given by NAME and evaluate BODY.
If the perspective NAME doesn't yet exists, create it.
If the perspective library is not available, just evaluate BODY."
  `(if (fboundp 'persp-mode)             ; persp library available
       (progn
         (unless persp-mode (persp-mode))
         (persp-switch ,name)
         ,@body)
     ,@body))

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

(defcustom persp-app-git-repositories-path "~/git"
  "Path to a directory of git repositories.
Used by `define-persp-magit-app'"
  :group 'persp-app)

(defmacro define-persp-magit-app (persp-name key)
  "Define a perspective application for a git project using `magit-status'."
  `(define-persp-app
     ,persp-name
     (magit-status ,(concat persp-app-git-repositories-path persp-name))
     ,key))

;; Custom Perspectives

(define-persp-magit-app "these" (kbd "C-c t"))
(define-persp-magit-app "dedukti" (kbd "C-c d"))
(define-persp-magit-app "sigmaid" (kbd "C-c ç"))
(define-persp-magit-app "expressing" (kbd "C-c e"))
(define-persp-magit-app "focalize" (kbd "C-c f"))
(define-persp-magit-app "holide" (kbd "C-c h"))
(define-persp-magit-app "zenon" (kbd "C-c z"))


;; 
(defun define-persp-with-git (name)
  "Interactive define-persp-magit-app with completion on files in `persp-app-git-repositories-path'."
  (interactive
   (list
    (completing-read "New magit perspective: " (directory-files persp-app-git-repositories-path))))
  (eval `(define-persp-magit-app ,name nil))
  (eval (read (concat "(persp-" name ")"))))

(global-set-key (kbd "C-c v") 'define-persp-with-git)


(defun define-persp-with-cmd (name)
  (interactive (list (read-shell-command "New command perspective: ")))
  (eval `(define-persp-app ,name
           (exwm-run ,name) nil))
  (eval (read (concat "(persp-" name ")"))))

(global-set-key (kbd "C-c !") 'define-persp-with-cmd)

(define-persp-app "config"
  (progn
    (find-file "~/.emacs")
    (find-file "~/elisp/local-conf.el")
    (dired "~/git/emacs-config/.emacs.d/elisp")
    (magit-status "~/git/emacs-config"))
  (kbd "C-c <menu>"))

(define-persp-app "gnus"
  (gnus)
  (kbd "C-c n"))

(eval-after-load 'exwm-conf
  '(progn
     (define-persp-app "wicd" (exwm-run "wicd-gtk") (kbd "C-c w"))
     (define-persp-app "web" (exwm-run "tbb") (kbd "C-c b"))))

(provide 'persp-conf)
;;; persp-conf.el ends here
