;;; Programming

;; Enforce 80 columns
(use-package column-enforce-mode
  :config (add-hook 'prog-mode-hook 'column-enforce-mode))

;; Modes for special languages
;; OCaml
(use-package tuareg
  :config (require 'tuareg-conf))

;; FoCaLiZe
; (require 'focalize)
; (load "~/git/focalize/focalizec/emacs/focalize.el")

;; Dedukti
(use-package dedukti-mode
  :config
  (setq dedukti-path (executable-find "dkcheck")
	dedukti-check-options '("-nc" "-r")
        dedukti-compile-options '("-nc" "-e" "-r"))
  (add-hook 'dedukti-mode-hook (lambda () (electric-pair-mode nil))))

(use-package flycheck-dedukti
  :config
  (add-hook 'dedukti-mode-hook (lambda ()
     (flycheck-select-checker 'dedukti)
     (flycheck-mode)
     )))

;; LaTeX
(use-package tex
  :config (require 'latex-conf))

;; Python
(use-package python
  :config (require 'python-conf))

;; Coq
(use-package proofgeneral
  :init (load "~/.emacs.d/elisp/PG/generic/proof-site")
  :config (require 'coq-conf))

;; GrassHopper
(load "~/.emacs.d/elisp/flycheck")
(load "~/.emacs.d/elisp/spl-mode")

;; OTT
(require 'ottmode)

;; Bind keys to functions jumping to source code
;; Idea from http://emacsredux.com/blog/2014/06/18/quickly-find-emacs-lisp-sources/
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)

;; Electric pairs
(setq electric-pair-mode t
      electric-pair-pairs '((?\" . ?\")
                            (?\( . ?\))
                            (?\{ . ?\})
                            (?\[ . ?\])))

;; Iedit-mode
(global-set-key (kbd "C-;") 'iedit-mode)

(use-package auto-highlight-symbol
  :config
  (setq  ahs-idle-interval 0.2)
  (add-hook 'prog-mode-hook 'auto-highlight-symbol-mode))

;; Delete many whitespace at once
(use-package hungry-delete
  :config (global-hungry-delete-mode))

;; Spell checking
(use-package ispell
  :config (setq ispell-program-name "aspell"))

(use-package flyspell
  :config
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(use-package guess-language
  :config
  (setq guess-language-languages '(en fr)
        guess-language-min-paragraph-length 35)
  (add-hook 'text-mode-hook 'guess-language-mode))

;; Typographic improvements
(use-package typo-mode
  :config
  (add-hook 'text-mode-hook 'typo-mode))

;; Checking
(use-package flycheck-mode
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode)
  (setq flyspell-auto-correct-binding (kbd "M-<tab>"))
  )

(provide 'editing-conf)
