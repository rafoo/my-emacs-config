;; Configuration of the completion mecanisms

(setq completion-auto-help 'lazy ;; Completion buffer after two tabs
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t )

;; Icomplete
(icomplete-mode 1)

;; IDO
(require 'ido)
(ido-mode t)

;; Auto-complete
(when (fboundp 'auto-complete)
  (add-hook 'prog-mode-hook 'auto-complete-mode)
  (setq ac-ignore-case nil))

(provide 'completion-conf)
