;; Configuration of org mode

; (require 'org-install)
; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
; (define-key global-map "\C-cl" 'org-store-link)
; (define-key global-map "\C-ca" 'org-agenda)
; (setq org-log-done t)

; (require 'org-special-blocks nil t)

; (require 'org-latex)
; (require 'org)

(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))

;(add-to-list 'org-latex-packages-alist
;             '( "utf8" . "inputenc" ))

;; (add-to-list ' org-latex-classes
;;              '("article"
;;                "\\documentclass{article}"
;;                ("\\section{%s}" . "\\section*{%s}")))

(add-to-list 'org-latex-classes
             '("logic-article"
               "\\documentclass{llncs}
[NO-DEFAULT-PACKAGES]
\\usepackage{amsmath}
%\\usepackage{a4wide}
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
\\usepackage{unicode-math}
\\setallmainfonts{FreeSerif}
\\setmathfont{xits-math.otf}
%\\usepackage[french]{babel} 
\\usepackage{bussproofs}
\\newcommand{\\myUIC}[3]
  {\\mbox{
     \\AxiomC{#2}
     \\RightLabel{\\scriptsize(#1)}
     \\UnaryInfC{#3}
     \\DisplayProof}}
\\newcommand{\\myBIC}[4]
  {\\mbox{
     \\AxiomC{#2}
     \\AxiomC{#3}
     \\RightLabel{\\scriptsize(#1)}
     \\BinaryInfC{#4}
     \\DisplayProof}}
\\newcommand{\\myTIC}[5]
  {\\mbox{
     \\AxiomC{#2}
     \\AxiomC{#3}
     \\AxiomC{#4}
     \\RightLabel{\\scriptsize(#1)}
     \\TrinaryInfC{#5}
     \\DisplayProof}}
\\newcommand{\\myQIC}[6]
  {\\mbox{
     \\AxiomC{#2}
     \\AxiomC{#3}
     \\AxiomC{#4}
     \\AxiomC{#5}
     \\RightLabel{\\scriptsize(#1)}
     \\QuaternaryInfC{#6}
     \\DisplayProof}}
\\usepackage{setspace}
\\usepackage{framed}
\\newenvironment{infset}
  {\\begin{center}
     \\begin{framed}
       \\setstretch{3}}
  {  \\end{framed}
   \\end{center}}
"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("article"
               "\\documentclass[11pt]{article}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))



(setq org-export-latex-hyperref-format "\\ref{%s}")

(defun my-latex-captions-bellow (link backend info)
  (when (string-match
         "^\\(\\\\begin{figure}\n\\)\\(\\\\caption.*\n\\)\\(\\(.*\n\\)*\\)\\(\\\\end{figure}\n$\\)"
         link)
    (replace-match "\\1\\3\\2\\5" nil nil link)))

(when (boundp 'org-export-filter-special-block-functions)
  (add-to-list 'org-export-filter-special-block-functions 'my-latex-captions-bellow))

(provide 'org-conf)
