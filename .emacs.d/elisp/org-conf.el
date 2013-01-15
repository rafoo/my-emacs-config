;; Configuration of org mode

; (require 'org-install)
; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
; (define-key global-map "\C-cl" 'org-store-link)
; (define-key global-map "\C-ca" 'org-agenda)
; (setq org-log-done t)

; (require 'org-special-blocks nil t)

; (require 'org-latex)
; (require 'org)

(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))

;(add-to-list 'org-export-latex-packages-alist
;             '( "utf8" . "inputenc" ))

;; (add-to-list ' org-export-latex-classes
;;              '("article"
;;                "\\documentclass{article}"
;;                ("\\section{%s}" . "\\section*{%s}")))

(add-to-list 'org-export-latex-classes
             '("logic-article"
               "\\documentclass[11pt]{article}
[NO-DEFAULT-PACKAGES]
\\usepackage{fixltx2e}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{wrapfig}
\\usepackage{soul}
\\usepackage{textcomp}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage{latexsym}
\\usepackage{amssymb}
\\usepackage{hyperref}
\\tolerance=1000
\\usepackage{mathspec}
\\setallmainfonts{FreeSerif}
\\usepackage[french]{babel} 
\\usepackage{bussproofs}
\\newcommand{\\myUIC}[3]{\\mbox{\\AxiomC{#2}\\RightLabel{\\scriptsize(#1)}\\UnaryInfC{#3}\\DisplayProof}}
\\newcommand{\\myBIC}[4]{\\mbox{\\AxiomC{#2}\\AxiomC{#3}\\RightLabel{\\scriptsize(#1)}\\BinaryInfC{#4}\\DisplayProof}}
\\newcommand{\\myTIC}[5]{\\mbox{\\AxiomC{#2}\\AxiomC{#3}\\AxiomC{#4}\\RightLabel{\\scriptsize(#1)}\\TrinaryInfC{#5}\\DisplayProof}}
\\newcommand{\\myQIC}[6]{\\mbox{\\AxiomC{#2}\\AxiomC{#3}\\AxiomC{#4}\\AxiomC{#5}\\RightLabel{\\scriptsize(#1)}\\QuaternaryInfC{#6}\\DisplayProof}}
\\usepackage{setspace}
\\usepackage{framed}
\\newenvironment{infset}{\\begin{center}\\begin{framed}\\setstretch{3}}{\\end{framed}\\end{center}}
"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))



(provide 'org-conf)