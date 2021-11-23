;;; init-keys.el --- Summary
;;; Commentary:
;;; init keys config
;;; Code:

(require 'init-osx-keys)
(require 'init-which-key)

(global-unset-key (kbd "C-x C-c"))
(global-set-key (kbd "C-x C-w") 'save-buffers-kill-terminal)

(provide 'init-keys)
;;; init-keys.el ends here
