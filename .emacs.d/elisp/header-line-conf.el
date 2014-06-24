;; Configuration of the header-line to display global informations such as time, battery, ...

(setq global-header-string)
(defmacro var-subst (var1 var2 exp)
  `(let ((,var1 ,var2))
        ,exp
        (setq ,var2 ,var1) ) )

(defun apply-header (fun &rest args)
  "Apply function FUN with arguments ARGS using `global-header-string'
in place of `global-mode-string'."
  (var-subst global-mode-string global-header-string (apply fun args)) )

;;; Global information

;; Time
(when (require 'time nil t)
  (setq display-time-default-load-average nil
        display-time-format "%T (%a %d %b)"
        display-time-interval 1
        display-time-use-mail-icon t
        display-time-mail-string "" )
  (apply-header 'display-time-mode 1) )

;; Wireless network
(when (require 'wireless nil t)
  (setq wireless-mode-line-format "[%n:%k,%l,%s]")
  (apply-header 'display-wireless-mode 1))

;;; More header-line

(defun my-header-line (format)
  "Concatenation of the argument FORMAT seen as a mode-line-format with some fixed information"
  (list "" format
        '(global-header-string (" " global-header-string)) ) )

(defun append-header ()
  (interactive)
  (setq header-line-format (my-header-line header-line-format)) )

(add-hook 'after-change-major-mode-hook 'append-header)

(provide 'header-line-conf)
