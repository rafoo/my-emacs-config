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

  ;; FlyMake
  (defun flymake-ocaml-init ()
    (flymake-simple-make-init-impl
     'flymake-create-temp-with-folder-structure nil nil
     (file-name-nondirectory buffer-file-name)
     'flymake-get-ocaml-cmdline))
  (defun flymake-get-ocaml-cmdline (source base-dir)
    (list "ocaml_flycheck.pl"
          (list source base-dir)))
  
  (push '(".+\\.ml[yilp]?$"
          flymake-simple-make-init
          flymake-simple-cleanup
          flymake-get-real-file-name)
        flymake-allowed-file-name-masks)
  (push
   '("^\\(\.+\.ml[yilp]?\\|\.lhs\\):\\([0-9]+\\):\\([0-9]+\\):\\(.+\\)"
     1 2 3 4) flymake-err-line-patterns)

  (add-hook
   'tuareg-mode-hook
   '(lambda ()
      (if (not (null buffer-file-name)) (flymake-mode))))

  (when (fboundp 'resize-minibuffer-mode) ; for old emacs
    (resize-minibuffer-mode)
    (setq resize-minibuffer-window-exactly nil))

  (defun credmp/flymake-display-err-minibuf () 
    "Displays the error/warning for the current line in the minibuffer"
    (interactive)
    (let* ((line-no             (flymake-current-line-no))
           (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
           (count               (length line-err-info-list))
           )
      (while (> count 0)
        (when line-err-info-list
          (let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
                 (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
                 (text (flymake-ler-text (nth (1- count) line-err-info-list)))
                 (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
            (message "[%s] %s" line text)
            )
          )
        (setq count (1- count)))))

  (add-hook
   'tuareg-mode-hook
   '(lambda ()
      (define-key tuareg-mode-map "\C-cd"
        'credmp/flymake-display-err-minibuf)))

  )

(provide 'tuareg-conf)
