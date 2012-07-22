
;; ProofGeneral, mode for proof assistants
;(load-file "~/elisp/isar-pg-env.el")
;(load-file "/home/harry/Isabelle/Isabelle2011/contrib/ProofGeneral/isar/interface-setup.el")
;(load-file "/home/harry/Isabelle/Isabelle2011/etc/isar-keywords.el")
;(load-file "/home/harry/Isabelle/Isabelle2011/etc/proofgeneral-settings.el")
;(load-file "/home/harry/Isabelle/Isabelle2011/contrib/ProofGeneral/generic/proof-site.el")
;(load-file "/home/harry/Isabelle/Isabelle2011/contrib/ProofGeneral/isar/isar.el")
;; TODO: généraliser

(setq isar-maths-menu-enable t
      isar-mmm-enable nil
      isar-tracing:auto-nitpick t
      isar-tracing:auto-sledgehammer t
      isar-tracing:auto-solve-direct t
      isar-tracing:auto-try t
      isar-tracing:trace-simplifier nil
      proof-autosend-enable nil
      proof-delete-empty-windows nil
      proof-disappearing-proofs t
      proof-imenu-enable nil
      proof-toolbar-enable nil)

(provide 'isabelle)
