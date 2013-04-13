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
  
  ; FlyMake
  (push '(".+\\.ml[yilp]?$"
          flymake-simple-init
          flymake-simple-cleanup
          flymake-get-real-file-name)
        flymake-allowed-file-name-masks)
  (push
   '("^\\(\.+\.ml[yilp]?\\|\.lhs\\):\\([0-9]+\\):\\([0-9]+\\):\\(.+\\)"
     1 2 3 4) flymake-err-line-patterns)

  )

(provide 'tuareg-conf)
