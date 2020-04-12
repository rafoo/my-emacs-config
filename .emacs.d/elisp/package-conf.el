;; redefine package-all-keywords because the default implementation is too slow
;; this one uses add-to-list to remove duplicates

(defun package-all-keywords ()
  "Return the list of keywords in all packages."
  (let (pc--keywords)
    (package--mapc
     (lambda (package)
       (mapc
        (lambda (pc--keyword) (add-to-list 'pc--keywords pc--keyword 'append))
        (package-desc--keywords package))))
    pc--keywords))


(defun package-packages-without-keywords ()
  "Return the list of packages without keywords."
  (let (pc--packages)
    (package--mapc
     (lambda (package)
       (unless (package-desc--keywords package)
         (add-to-list 'pc--packages (package-desc-name package)))
       ))
    pc--packages))

; (package-show-package-list (package-packages-without-keywords))
