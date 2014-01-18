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

;; Activate compilation-shell-minor-mode to jump to files
(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

(global-set-key (kbd "C-c s") 'eshell)

(provide 'eshell-conf)
