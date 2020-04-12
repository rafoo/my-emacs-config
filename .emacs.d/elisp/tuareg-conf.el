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
(use-package ocp-indent
  :config
  (add-hook 'before-save-hook
            (lambda ()
              (when (and (stringp buffer-file-name)
                         (string-match "\\.ml[ily]?\\'" buffer-file-name))
                ;; remove l and y from the regex
             (ocp-indent-buffer)))))

(setq ocp-indent-path "ocamlformat")

(provide 'tuareg-conf)
