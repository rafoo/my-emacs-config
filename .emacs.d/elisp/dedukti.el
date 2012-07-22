;; Generic and dedukti modes
(require 'generic-x)
(define-generic-mode
  'dedukti-mode
  '(("(;".";)"))                                    ;; comments
  '("-->" "[" "]" "." "->" "=>" ":")
  '(("[_a-zA-Z0-9]+" . 'font-lock-variable-face))
  '(".dk\\'")                                             ;; use this mode for .fcl files
  nil
  "Major mode for editing Dedukti source code files.")

(provide 'dedukti)