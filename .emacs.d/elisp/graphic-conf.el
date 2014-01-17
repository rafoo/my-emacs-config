;; Configuration for graphical Emacs session

;; Windows and panes
(require 'windows-conf)

;; Menu bar
(eval-after-load "menu-bar" '(require 'menu-bar+))

;; Tool-bar
(add-hook 'after-change-major-mode-hook
          (lambda () (tool-bar-mode 0)))

;; Cursor depending on mode (insert vs. overwrite)
;; From a comment from
;; http://emacs-fu.blogspot.fr/2009/12/changing-cursor-color-and-shape.html
(defvar hcz-set-cursor-type-type t)
(defvar hcz-set-cursor-type-buffer t)
(defun hcz-set-cursor-type-according-to-mode ()
  "change cursor type according to some minor modes."
  ;; setq cursor-type is somewhat costly, so we only call it when needed:
  (let ((type
         (if buffer-read-only 'hbar
           (if overwrite-mode 'box
             'bar))))
    (unless (and
             (string= type hcz-set-cursor-type-type)
             (string= (buffer-name) hcz-set-cursor-type-buffer))
      (setq cursor-type (setq hcz-set-cursor-type-type type))
      (setq hcz-set-cursor-type-buffer (buffer-name)))
    )
  )
(add-hook 'post-command-hook 'hcz-set-cursor-type-according-to-mode) 

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

;; Color theme
(load-theme 'zenburn t)

(provide 'graphic-conf)
