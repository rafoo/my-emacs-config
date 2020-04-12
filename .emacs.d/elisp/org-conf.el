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
(setq org-agenda-files '("~/org/agenda.org" "~/org/startup.org" "~/git/papiers_vecolib/biblio.org")
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

;; Make an easy template <S inserting a special block after querying
;; the corresponding LaTeX environment

(add-to-list 'org-structure-template-alist '("S" "#+BEGIN_%s\n?\n#+END_%s"))

;; Org templates do not have prompts except for file names so we
;; redefine an org function so that "%s" in the template is understood
;; as a string to be prompted.

(defun org-complete-expand-structure-template (start cell)
  "Expand a structure template."
  (let ((rpl (nth 1 cell))
	(ind ""))
    (delete-region start (point))
    (when (string-match "\\`[ \t]*#\\+" rpl)
      (cond
       ((bolp))
       ((not (string-match "\\S-" (buffer-substring (point-at-bol) (point))))
	(setq ind (buffer-substring (point-at-bol) (point))))
       (t (newline))))
    (setq start (point))
    (when (string-match "%file" rpl)
      (setq rpl (replace-match
		 (concat
		  "\""
		  (save-match-data
		    (abbreviate-file-name (read-file-name "Include file: ")))
		  "\"")
		 t t rpl)))
    (when (string-match "%s" rpl)
      (let ((replacement
             (save-match-data
               (abbreviate-file-name (read-string "LaTeX Environment: ")))))
        (while (string-match "%s" rpl)
          (setq rpl (replace-match replacement t t rpl)))))
    (setq rpl (mapconcat 'identity (split-string rpl "\n")
			 (concat "\n" ind)))
    (insert rpl)
    (when (re-search-backward "\\?" start t) (delete-char 1))))


(defun org-block-split-block ()
  (interactive)
  (forward-line 0)
  (let (previous next)
    (save-excursion
      (re-search-backward "#\\+BEGIN_.*$")
      (setq previous (match-string 0)))
    (save-excursion
      (re-search-forward "#\\+END_[a-zA-Z]+")
      (setq next (match-string 0)))
    (unless (bolp) (insert "\n"))
    (insert next "\n")
    (save-excursion (insert "\n" previous "\n"))))

(define-key org-mode-map (kbd "C-c _") 'org-block-split-block)

(provide 'org-conf)
