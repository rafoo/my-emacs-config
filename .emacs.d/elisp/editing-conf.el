;;; Programming

;; FlyMake : on-the-fly syntax checking
;; (require 'flymake)
;; (add-hook 'find-file-hook 'flymake-find-file-hook)

;; Auto-complete
(require 'auto-complete)
(add-hook 'prog-mode-hook 'auto-complete-mode)

;; Modes for special languages
(require 'focalize) ; FoCaLiZe
(require 'dedukti) ; Dedukti
(require 'latex-conf) ; LaTeX
(require 'org-conf) ; Org mode
; (require 'isabelle) ; Isabelle
(require 'python-conf)

(provide 'editing-conf)
