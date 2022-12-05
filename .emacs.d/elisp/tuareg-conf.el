;; Tuareg OCaml mode

(setq tuareg-font-lock-symbols t)

;; From Merlin installation message
(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var"
                                                     "share")))))
  (when (and opam-share (file-directory-p opam-share))
    ;; Register Merlin
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    ;; Automatically start it in OCaml buffers
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command 'opam)))

(add-to-list 'load-path "/home/cauderlier/git/tezos/_opam/share/emacs/site-lisp")

(use-package ocamlformat
  :config
  (add-hook 'before-save-hook 'ocamlformat-before-save))

(provide 'tuareg-conf)
