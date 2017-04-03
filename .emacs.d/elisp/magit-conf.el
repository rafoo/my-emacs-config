

(setq magit-last-seen-setup-instructions "1.4.0")

;; Unbind <s-tab> so that its global binding can be used instead
;; magit-section-cycle-global is still accessible via S-tab
(define-key magit-mode-map (kbd "<s-tab>") nil)

(provide 'magit-conf)
