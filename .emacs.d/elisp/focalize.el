;; Generic and focalize modes
(require 'generic-x)
(define-generic-mode
  'focalize-mode
  '(("(*"."*)") ("--"))                                    ;; comments
  '("alias" "all" "and" "as" "assume" "assumed"
    "begin" "by"
    "caml" "collection" "conclude" "coq" "coq_require"
    "definition"
    "else" "end" "ex" "external"
    "false" "function"
    "hypothesis"
    "if" "in" "inherit" "internal" "implement" "is"
    "let" "lexicographic" "local" "logical"
    "match" "measure"
    "not" "notation"
    "of" "on" "open" "or" "order"
    "proof" "prop" "property" "prove"
    "qed"
    "rec" "representation"
    "Self" "signature" "species" "step" "structural"
    "termination" "then" "theorem" "true" "type"
    "use"
    "with")                                                ;; keywords 
  '(("_*[A-Z][_a-zA-Z0-9]*" . 'font-lock-constant-face)
    ("_*[a-z0-9][_a-zA-Z0-9]*". 'font-lock-variable-name-face)) ;; identifiers
  '(".fcl\\'")                                             ;; use this mode for .fcl files
  nil
  "Major mode for editing FoCaLiZe (http://focalize.inria.fr/) source code files.")

(provide 'focalize)
