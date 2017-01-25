;; Configuration for coq-mode and proofgeneral

(setq coq-load-path '("/usr/local/lib/focalize" "/usr/local/lib/zenon")
      proof-disappearing-proofs t
      proof-electric-terminator-enable t
      proof-three-window-mode-policy 'hybrid)

(global-set-key (kbd "C-c RET") 'proof-goto-point)

(provide 'coq-conf)
