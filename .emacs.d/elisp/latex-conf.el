;; LaTeX configuration : use evince as pdf-reader
(when (require 'tex nil t)
  (setq TeX-output-view-style
	'(("^dvi$" ("^landscape$" "^pstricks$\\|^pst-\\|^psfrag$") "%(o?)dvips -t landscape %d -o && gv %f")
	  ("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "%(o?)dvips %d -o && gv %f")
	  ("^dvi$" ("^\\(?:a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4\\)$" "^landscape$") "%(o?)xdvi %dS -paper a4r -s 0 %d")
	  ("^dvi$" "^\\(?:a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4\\)$" "%(o?)xdvi %dS -paper a4 %d")
	  ("^dvi$" ("^\\(?:a5\\(?:comb\\|paper\\)\\)$" "^landscape$") "%(o?)xdvi %dS -paper a5r -s 0 %d")
	  ("^dvi$" "^\\(?:a5\\(?:comb\\|paper\\)\\)$" "%(o?)xdvi %dS -paper a5 %d")
	  ("^dvi$" "^b5paper$" "%(o?)xdvi %dS -paper b5 %d")
	  ("^dvi$" "^letterpaper$" "%(o?)xdvi %dS -paper us %d")
	  ("^dvi$" "^legalpaper$" "%(o?)xdvi %dS -paper legal %d")
	  ("^dvi$" "^executivepaper$" "%(o?)xdvi %dS -paper 7.25x10.5in %d")
	  ("^dvi$" "." "%(o?)xdvi %dS %d")
	  ("^pdf$" "." "evince %o") ;; instead of default xpdf
	  ("^html?$" "." "netscape %o")))
  (setq preview-required-option-list
	'("active" "tightpage" (preview-preserve-counters "counters")))
  (TeX-global-PDF-mode 1))

(provide 'latex-conf)