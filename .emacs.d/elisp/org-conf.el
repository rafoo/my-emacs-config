;; Configuration of org mode

(define-key global-map "\C-cl" 'org-store-link)

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



(setq org-export-latex-hyperref-format "\\ref{%s}")

(defun my-latex-captions-bellow (link backend info)
  (when (string-match
         "^\\(\\\\begin{figure}\n\\)\\(\\\\caption.*\n\\)\\(\\(.*\n\\)*\\)\\(\\\\end{figure}\n$\\)"
         link)
    (replace-match "\\1\\3\\2\\5" nil nil link)))

(when (boundp 'org-export-filter-special-block-functions)
  (add-to-list 'org-export-filter-special-block-functions 'my-latex-captions-bellow))
(setq org-agenda-files '("~/zamok/org/todo.org" "~/org/todo.org")
      org-agenda-span 14
      org-export-latex-default-packages-alist '(("" "fixltx2e" nil)
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
      org-export-latex-packages-alist nil
      org-latex-listings t
      org-latex-pdf-process '("xelatex -interaction nonstopmode -output-directory %o %f"
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
                     org-w3m
                     org-special-blocks)
      org-src-fontify-natively t)

;; New names for some variables
(setq org-latex-default-packages-alist org-export-latex-default-packages-alist
      org-latex-packages-alist org-export-latex-packages-alist
      org-latex-to-pdf-process org-latex-pdf-process)

(provide 'org-conf)
