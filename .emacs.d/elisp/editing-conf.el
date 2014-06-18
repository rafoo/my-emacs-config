;;; Programming

;; FlyMake : on-the-fly syntax checking
;; (require 'flymake)
;; (add-hook 'find-file-hook 'flymake-find-file-hook)

;; Enforce 80 columns
(when (require 'column-enforce-mode nil t)
  (add-hook 'prog-mode-hook 'column-enforce-mode))



;; Modes for special languages
;(require 'tuareg-conf) ; OCaml
;(require 'focalize) ; FoCaLiZe
;(require 'dedukti) ; Dedukti
(require 'latex-conf) ; LaTeX
(require 'org-conf) ; Org mode
; (require 'isabelle) ; Isabelle
(require 'python-conf)

(require 'coq-conf)

(provide 'editing-conf)
