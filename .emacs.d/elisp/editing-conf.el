;;; Programming

;; FlyMake : on-the-fly syntax checking
;; (require 'flymake)
;; (add-hook 'find-file-hook 'flymake-find-file-hook)

;; Enforce 80 columns
(when (fboundp 'column-enforce-mode)
  (add-hook 'prog-mode-hook 'column-enforce-mode))

;; Modes for special languages
(eval-after-load "tuareg"
  '(require 'tuareg-conf)) ; OCaml
;(require 'focalize) ; FoCaLiZe
(eval-after-load "tex"
  '(require 'latex-conf)) ; LaTeX
; (require 'isabelle) ; Isabelle
(eval-after-load "python"
  '(require 'python-conf))
(eval-after-load "proofgeneral"
  '(require 'coq-conf))

(require 'ottmode)

;; Bind keys to functions jumping to source code
;; Idea from http://emacsredux.com/blog/2014/06/18/quickly-find-emacs-lisp-sources/
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)

;; Iedit-mode
(global-set-key (kbd "C-;") 'iedit-mode)

;; Spell checking
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(add-hook 'prog-mode-hook 'auto-highlight-symbol-mode)

;; Checking
(when (fboundp 'flycheck-mode)
  (add-hook 'prog-mode-hook 'flycheck-mode))

(provide 'editing-conf)
