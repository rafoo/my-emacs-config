;;; Programming

;; FlyMake : on-the-fly syntax checking
;; (require 'flymake)
;; (add-hook 'find-file-hook 'flymake-find-file-hook)

;; Auto-complete
(when (require 'auto-complete nil t)
  (add-hook 'prog-mode-hook 'auto-complete-mode))

;; Modes for special languages
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
