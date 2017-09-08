;;; init-which-key.el --- Summary
;;; Commentary:
;;; init dired config
;;; Code:

(use-package which-key
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-popup-type 'minibuffer))

(provide 'init-which-key)
;;; init-which-key.el ends here
