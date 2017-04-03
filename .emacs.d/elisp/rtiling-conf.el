;;; rtiling-conf.el --- Dynamic tiling window management

;;; Code:

(autoload #'persp-buffers "perspective.el")

(require 'perspective)
(require 'rtiling)

(defun persp-list-buffers ()
  "List buffers in the current perspective."
  (persp-buffers persp-curr))

(setq rtiling-buffer-listing-function #'persp-list-buffers)

(provide 'rtiling-conf)
;;; rtiling-conf.el ends here
