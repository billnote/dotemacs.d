;;; init-command-log.el --- Summary
;;; Commentary:
;;; init command log
;;; Code:

(use-package command-log-mode
  :init
  (setq clm/logging-dir (expand-file-name "logs/command/" user-emacs-directory)))


(provide 'init-command-log)
;;; init-command-log.el ends here
