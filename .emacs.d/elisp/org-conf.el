;; Configuration of org mode

(define-key global-map "\C-cl" 'org-store-link)

;; Capture
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates nil)

(add-to-list 'org-capture-templates
             '("r"
               "Rendez-vous"
               entry
               (file+headline "~/org/agenda.org" "Rendez-vous")
               "* %:fromname\n    SCHEDULED: %^T"))
(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))

(add-to-list 'org-latex-classes
             `("logic-article"
               ,(with-temp-buffer
                  (insert-file-contents "~/git/emacs-config/.emacs.d/elisp/logic-article-prelude.tex")
                  (buffer-string))
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

(add-to-list 'org-latex-classes
             '("report"
               "\\documentclass[11pt]{report}"
               ("\\part{%s}" . "\\part*{%s}")
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("book"
               "\\documentclass[a4paper,11pt]{book}\n\\pdfobjcompresslevel 0\n\\usepackage[a-1b]{pdfx}"
               ("\\mypart{%s}" . "\\part*{%s}")
               ("\\myregchapter{%s}" . "\\mychapter{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("sigplan"
               "\\documentclass[preprint,10pt]{sigplanconf}"
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
(setq org-agenda-files '("~/org/todo.org.gpg" "~/org/agenda.org" "~/org/startup.org" "~/git/papiers_vecolib/biblio.org")
      org-agenda-span 14
      org-latex-default-packages-alist '(("" "amsmath" t)
                                         ("" "graphicx" t)
                                         ("" "longtable" nil)
                                         ("" "float" nil)
                                         ("" "wrapfig" nil)
                                         ("" "soul" t)
                                         ("" "textcomp" t)
                                         ("" "marvosym" t)
                                         ("" "wasysym" t)
                                         ("" "latexsym" t)
                                         ("" "amssymb" t)
                                         ("" "hyperref" nil)
                                         "\\tolerance=1000")
      org-latex-packages-alist nil
      org-latex-listings t
      org-latex-to-pdf-process '("xelatex -interaction nonstopmode -output-directory %o %f"
                              "bibtex %b"
                              "xelatex -interaction nonstopmode -output-directory %o %f"
                              "xelatex -interaction nonstopmode -output-directory %o %f")
      org-mobile-directory "~/zamok/org/mobile/"
      org-modules  '(org-bbdb
                     org-bibtex
                     org-docview
                     org-gnus
                     org-info
                     org-jsinfo
                     org-irc
                     org-mew
                     org-mhe
                     org-rmail
                     org-vm
                     org-wl
                     org-w3m)
      org-src-fontify-natively t)

(provide 'org-conf)
