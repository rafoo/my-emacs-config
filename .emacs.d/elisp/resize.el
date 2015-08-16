;; Resize windows using shortcuts

(defun v-resize (key)
  "interactively resize the window"
  (interactive "cHit +/-/</> to enlarge/shrink")
  (cond
   ((eq key (string-to-char "+"))
    (enlarge-window 1)
    (call-interactively 'v-resize))
   ((eq key (string-to-char "-"))
    (enlarge-window -1)
    (call-interactively 'v-resize))
   ((eq key (string-to-char ">"))
    (enlarge-window-horizontally 1)
    (call-interactively 'v-resize))
   ((eq key (string-to-char "<"))
    (enlarge-window-horizontally -1)
    (call-interactively 'v-resize))
   (t (push key unread-command-events))))

(provide 'resize)
