;;; Programming

;; Enforce 80 columns
(use-package column-enforce-mode
  :config (add-hook 'prog-mode-hook 'column-enforce-mode))

;; Keep parens balanced
(use-package smartparens
  :commands sp-with-modes
  :config
  (require 'smartparens-config)
  (add-hook 'prog-mode-hook 'smartparens-mode))




;; Modes for special languages
;; OCaml
(use-package tuareg
  :config (require 'tuareg-conf))

;; FoCaLiZe
; (require 'focalize)
; (load "~/git/focalize/focalizec/emacs/focalize.el")

;; Dedukti
(use-package flycheck-dedukti)

(use-package dedukti-mode
  :config
  (setq dedukti-path (executable-find "dkcheck")
	dedukti-check-options '("-nc" "-r")
        dedukti-compile-options '("-nc" "-e" "-r"))
  ;; Activate smartparens
  (add-hook 'dedukti-mode-hook 'smartparens-mode)
;  (sp-with-modes '(dedukti-mode) (sp-local-pair "(;" ";)" )) ;; commented because sp-local-pair does not seem to exist anymore (Debugger entered--Lisp error: (void-function sp-local-pair))
  ;; Activate flycheck
  (add-hook 'dedukti-mode-hook 'flycheck-dedukti-hook)
  )

(use-package michelson-mode
  :config (require 'michelson-conf))

;; LaTeX
(use-package tex
  :config (require 'latex-conf))

;; Python
(use-package python
  :config (require 'python-conf))

;; Coq
(use-package proof-general
  :init
  :config (require 'coq-conf))

;; ;; GrassHopper
;; (load "~/.emacs.d/elisp/flycheck")
;; (load "~/.emacs.d/elisp/spl-mode")

;; OTT
(require 'ottmode)

;; Bind keys to functions jumping to source code
;; Idea from http://emacsredux.com/blog/2014/06/18/quickly-find-emacs-lisp-sources/
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)

;; Iedit-mode
(global-set-key (kbd "C-;") 'iedit-mode)

(use-package auto-highlight-symbol
  :config
  (setq  ahs-idle-interval 0.2)
  (add-hook 'prog-mode-hook 'auto-highlight-symbol-mode))

;; Spell checking
(setq my-ispell-program-name
      (or
       (executable-find "aspell")
       (executable-find "ispell")))

(use-package ispell
  :if my-ispell-program-name
  :config
  (setq ispell-program-name my-ispell-program-name))

(use-package flyspell
  :if my-ispell-program-name
  :config
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(use-package guess-language
  :config
  (setq guess-language-languages '(en fr)
        guess-language-min-paragraph-length 35)
  (add-hook 'text-mode-hook 'guess-language-mode))

;; Checking
(use-package flycheck
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode)
  (setq flyspell-auto-correct-binding (kbd "M-<tab>"))
  )

;; Folding
(use-package origami
  :config
  (global-origami-mode)
  (define-key origami-mode-map (kbd "C-<tab>") 'origami-toggle-all-nodes)
  (define-key origami-mode-map (kbd "M-<tab>") 'origami-recursively-toggle-node))

;; Smart tabs
(require 'smart-tab)
(use-package smart-tab
  :config
  (global-smart-tab-mode 1))



(provide 'editing-conf)
