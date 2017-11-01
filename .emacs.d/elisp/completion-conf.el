;; Configuration of the completion mecanisms

(setq completion-auto-help 'lazy ;; Completion buffer after two tabs
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      tab-always-indent 'complete)

;; IDO
(use-package ido
  :config (ido-mode t))

(use-package ido-at-point
  :config (ido-at-point-mode 1))

(use-package ido-completing-read+
  :config (ido-ubiquitous-mode 1))

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-disable-if-short nil
        ido-vertical-pad-list t
        ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

  ;; Ido-vertical-mode is too slow for describe-function
  ;; It is not yet possible to deactivate it on a per-function basis
  ;; (https://github.com/creichert/ido-vertical-mode.el/issues/24)
  ;; so we deactivate ido for describe-function
  (add-to-list 'ido-cr+-function-blacklist 'describe-function))

;; Auto-complete
(use-package auto-complete
  :config
  (add-hook 'prog-mode-hook 'auto-complete-mode)
  (setq ac-ignore-case nil))

(provide 'completion-conf)
