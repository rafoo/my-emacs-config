(setq dired-listing-switches "-lrth --time-style=+%D%6R"
      wdired-allow-to-change-permissions t)

(use-package dired-quick-sort
  :config (dired-quick-sort-setup))


(provide 'dired-conf)
