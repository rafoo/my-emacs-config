;; Tuareg OCaml mode
(when (file-exists-p "/usr/share/emacs/site-lisp/tuareg-mode/tuareg.el")
  (load-file "/usr/share/emacs/site-lisp/tuareg-mode/tuareg.el")
  (add-to-list 'auto-mode-alist '("\\.ml[iylp]?" . tuareg-mode))
  (autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
  (autoload 'camldebug "camldebug" "Run the Caml debugger" t)
  (dolist (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi"))
    (add-to-list 'completion-ignored-extensions ext))
  (setq tuareg-font-lock-symbols t
        tuareg-in-indent 0)
  )

(provide 'tuareg-conf)