(setq magit-last-seen-setup-instructions "1.4.0")

;; Unbind <s-tab> so that its global binding can be used instead
;; magit-section-cycle-global is still accessible via S-tab
(define-key magit-mode-map (kbd "<s-tab>") nil)

;; For the diff fragment the point is in, highlight the difference
(setq magit-diff-refine-hunk t)

;; Taken from Magit manual, 5.1 Status Buffer
;; This allows to launch magit-status from ido
(eval-after-load 'ido
  (add-hook 'ido-setup-hook
            (lambda ()
              (define-key ido-completion-map
                (kbd "C-x g") 'ido-enter-magit-status))))

;; Configuration of M-x magit-list-repositories
(setq magit-repository-directories '(("~/git/" . 1)))


;; Activates the C-c M-g and C-x M-g popups.
;;
;; C-c M-g is for magit functions working on the file (stage, unstage,
;; diff, blame, etc...)
;;
;; C-x M-g is for magit functions working on the repository (pull,
;; push, etc...)
;;
(global-magit-file-mode 1)

(provide 'magit-conf)
