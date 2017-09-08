;;; init-smex.el --- Summary
;;; Commentary:
;;; init smex config
;;; Code:

;; Use smex to handle M-x
(use-package smex
  :config
  (setq-default smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  (global-set-key [remap execute-extended-command] 'smex))

(provide 'init-smex)
;;; init-smex.el ends here
