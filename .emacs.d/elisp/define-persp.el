;;; define-persp.el --- Define perspectives bound to Emacs applications
;;
;; Filename: define-persp.el
;; Description: Define perspectives bound to Emacs applications
;; Author: Raphaël Cauderlier
;; Maintainer: Raphaël Cauderlier
;; Copyright (C) 2014, Raphaël Cauderlier.
;; Version: 0.1
;;
;;; Commentary:
;;
;; This is not part of Emacs.
;;
;; This package provides ways to easily define new perspectives
;; running Emacs applications.  Here are some examples:
;;
;; - define a perspective associated with Gnus and bind it to "C-c n":
;; (define-persp-app "gnus" (gnus) (kbd "C-c n"))
;;
;; - define a perspective associated with ERC and bind it to "C-c i";
;;   the command (erc) is called only at creation of the perspective,
;;   not when we switch to it again:
;; (define-persp-app "erc" () (kbd "C-c i") (erc))
;;
;; - define a perspective associated with the package manager and bind
;;   it to "C-c p":
;; (define-persp-app "packages" (list-packages) (kbd "C-c p"))
;;
;; - define a "web" perspective for launching Firefox and bind it to
;;   "C-c f":
;; (define-persp-with-shell-process "web" "firefox" (kbd "C-c f"))
;;
;; Moreover, this packages extends the perspective-map by two new commands:
;;
;;
;; key       binding
;; ---       -------
;;
;; C-x x !   define-persp-with-cmd
;; C-x x g   define-persp-with-git
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Code:

(defmacro define-persp-run (name &rest body)
  "Switch to the perspective given by NAME and evaluate BODY.
If the perspective NAME doesn't yet exists, create it.
If the perspective library is not available, just evaluate BODY."
  `(if (fboundp 'persp-mode)             ; persp library available
       (progn
         (unless persp-mode (persp-mode))
         (persp-switch ,name)
         ,@body)
     ,@body))

(defmacro define-persp-app (persp-name form &optional key first-form)
  "Define a command persp- PERSP-NAME by wrapping FORM by `define-persp-run'.
If KEY is non nil, bind it to this command.  If FIRST-FORM is non
nil, call it before FORM when perspective is created."
  (let ((persp-command (intern (concat "persp-" persp-name))))
    (list 'progn
     `(defun ,persp-command ()
        ,(format "Run %s in a dedicated perspective." persp-name)
        (interactive)
        (define-persp-run ,persp-name
                       ,(if first-form
                          `(unless (and (fboundp 'persp-names)
                                        (member ,persp-name (persp-names)))
                             ,first-form ,form)
                          form)))
     (when key
       `(global-set-key ,key ',persp-command)))))

(defcustom define-persp-git-repositories-path "~/git/"
  "Path to a directory of git repositories.
Used by `define-persp-magit-app' and `define-persp-with-git'."
  :group 'persp-app)

(defmacro define-persp-magit-app (persp-name &optional key)
  "Define a perspective application for a git project using `magit-status'.
The string `PERSP-NAME' is both the name of the git directory in
`define-persp-git-repositories-path' and the name of the created
perspective.  If KEY is non nil, bind it to this perspective."
  `(define-persp-app
     ,persp-name
     (magit-status ,(concat define-persp-git-repositories-path persp-name))
     ,key))

(defun define-persp-with-git (name)
  "Interactive `define-persp-magit-app' with completion.
NAME is both the name of the git directory in
`define-persp-git-repositories-path' and the name of the created
perspective."
  (interactive
   (list
    (completing-read "New magit perspective: " (directory-files define-persp-git-repositories-path))))
  (eval `(define-persp-magit-app ,name))
  (eval (read (concat "(persp-" name ")"))))

(defmacro define-persp-with-shell-process (persp-name cmd &optional key)
    "Define a perspective application for a shell command.
The string PERSP-NAME is the name of the created perspective.
The string CMD is the shell command.  The command is called in
its own process using `start-process-shell-command'.  If KEY is
non nil, bind it to this perspective."
    `(define-persp-app
       ,persp-name
       (start-process-shell-command ,persp-name nil ,cmd)
       ,key))

(defun define-persp-with-cmd (name)
  "Define a perspective application for a shell command.
The string NAME is both the shell command and the name of the
created perspective.  The command is called in its own process
using `start-process-shell-command'."
  (interactive (list (read-shell-command "New command perspective: ")))
  (eval `(define-persp-with-shell-process ,name ,name))
  (eval (read (concat "(persp-" name ")")))
  )

(eval-after-load 'perspective
  '(progn
     (define-key perspective-map (kbd "!") #'define-persp-with-cmd)
     (define-key perspective-map (kbd "g") #'define-persp-with-git)))

(provide 'define-persp)
;;; define-persp.el ends here
