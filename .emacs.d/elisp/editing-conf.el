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

;; Bind keys to functions jumping to source code
;; Idea from http://emacsredux.com/blog/2014/06/18/quickly-find-emacs-lisp-sources/
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)

(provide 'editing-conf)
