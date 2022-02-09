;;; init-dashboard.el --- Summary
;;; Commentary:
;;; init dashboard config
;;; Code:

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title (concat "Happy hacking, " user-login-name " - Emacs ♥ you!"))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (setq dashboard-items '((recents  . 10)
                          (projects . 5)
                          (bookmarks . 5)
                          (agenda . 5)
                          (registers . 5)))
  ;; fuck the network greatwall
  ;; (use-package dashboard-hackernews
  ;;   :config
  ;;   (add-to-list 'dashboard-items '(hackernews . 10)))
  )
(provide 'init-dashboard)

;;; init-dashboard.el ends here
