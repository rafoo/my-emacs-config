;;; Programming

;; FlyMake : on-the-fly syntax checking
<<<<<<< HEAD
;(require 'flymake)
;(add-hook 'find-file-hook 'flymake-find-file-hook)
=======
;; (require 'flymake)
;; (add-hook 'find-file-hook 'flymake-find-file-hook)
>>>>>>> 7021362bf630c0ff4b857c1ac452cd9ac5908aec

;; Auto-complete
;(require 'auto-complete)
;(add-hook 'prog-mode-hook 'auto-complete-mode)

;; Modes for special languages
<<<<<<< HEAD
;(require 'tuareg-conf) ; OCaml
;(require 'focalize) ; FoCaLiZe
;(require 'dedukti) ; Dedukti
(require 'latex-conf) ; LaTeX
(require 'org-conf) ; Org mode
; (require 'isabelle) ; Isabelle
(require 'python-conf)

;;lua
;(add-to-list 'load-path "~/elisp/lua-mode/")
;(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
;(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
;(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))



(provide 'editing-conf)
