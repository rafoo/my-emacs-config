;; Configuration of the completion mecanisms

(setq completion-auto-help 'lazy ;; Completion buffer after two tabs
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      tab-always-indent 'complete)

;; Icomplete
(icomplete-mode 1)

;; IDO
(use-package ido
  :config (ido-mode t))

(use-package ido-at-point
  :config (ido-at-point-mode 1))

(use-package ido-completing-read+
  :config (ido-ubiquitous-mode 1))

(use-package ido-vertical-mode
  :config (ido-vertical-mode 1))

;; Auto-complete
(use-package auto-complete
  :config
  (add-hook 'prog-mode-hook 'auto-complete-mode)
  (setq ac-ignore-case nil))

(provide 'completion-conf)
